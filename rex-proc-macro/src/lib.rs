use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Type};

#[proc_macro_derive(Rex)]
pub fn derive_rex(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let name = &ast.ident;
    let name_as_str = format!("{name}");
    let docs = docs_from_attrs(&ast.attrs);

    let r#impl = match &ast.data {
        Data::Struct(data) => match &data.fields {
            // Turns into an ADT with one dictionary variant
            Fields::Named(named) => {
                let adt_variant = fields_named_to_adt_variant(&docs, &name_as_str, named);
                quote!(
                    ::rex_type_system::types::Type::ADT(::rex_type_system::types::ADT {
                        doc: #docs,
                        name: String::from(#name_as_str),
                        variants: vec![#adt_variant],
                    })
                )
            }
            // Turns into an ADT with one tuple variant (we drop the tuple if it
            // only has one element)
            Fields::Unnamed(unnamed) => {
                let adt_variant = fields_unnamed_to_adt_variant(&docs, &name_as_str, unnamed);
                quote!(
                    ::rex_type_system::types::Type::ADT(::rex_type_system::types::ADT {
                        doc: #docs,
                        name: String::from(#name_as_str),
                        variants: vec![#adt_variant],
                    })
                )
            }
            _ => quote! {
                ::rex_type_system::types::Type::ADT(::rex_type_system::types::ADT {
                    doc: #docs,
                    name: String::from(#name_as_str),
                    variants: vec![],
                })
            },
        },
        Data::Enum(data) => {
            let variants = data.variants.iter().map(|variant| {
                let variant_docs = docs_from_attrs(&variant.attrs);
                let mut variant_name = format!("{}", variant.ident.clone());
                rename_variant(&mut variant_name, variant);

                match &variant.fields {
                    Fields::Unnamed(unnamed) => {
                        fields_unnamed_to_adt_variant(&variant_docs, &variant_name, unnamed)
                    }
                    Fields::Named(named) => {
                        fields_named_to_adt_variant(&variant_docs, &variant_name, named)
                    }
                    Fields::Unit => quote! {
                        ::rex_type_system::types::ADTVariant {
                            doc: #variant_docs,
                            name: String::from(#variant_name),
                            t: None,
                            field_docs: None,
                        }
                    },
                }
            });
            quote! {
                ::rex_type_system::types::Type::ADT(::rex_type_system::types::ADT {
                    doc: #docs,
                    name: String::from(#name_as_str),
                    variants: vec![#(#variants,)*],
                })
            }
        }
        _ => panic!("Rex can only be derived for structs and enums"),
    };

    let generics = ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics ::rex_type_system::types::ToType for #name #ty_generics #where_clause {
            fn to_type() -> ::rex_type_system::types::Type {
                #r#impl
            }
        }
    };

    TokenStream::from(expanded)
}

fn fields_unnamed_to_adt_variant(
    variant_docs: &proc_macro2::TokenStream,
    variant_name: &str,
    fields: &FieldsUnnamed,
) -> proc_macro2::TokenStream {
    let ts = fields
        .unnamed
        .iter()
        .map(|field| {
            let t = to_type(&field.ty);
            quote! {
                #t
            }
        })
        .collect::<Vec<_>>();
    if ts.len() == 0 {
        quote!(
            ::rex_type_system::types::ADTVariant {
                doc: #variant_docs,
                name: String::from(#variant_name),
                t: None,
                field_docs: None,
            }
        )
    } else if ts.len() == 1 {
        let t = &ts[0];
        quote!(
            ::rex_type_system::types::ADTVariant {
                doc: #variant_docs,
                name: String::from(#variant_name),
                t: Some(Box::new(#t)),
                field_docs: None,
            }
        )
    } else {
        quote!({
            let mut elems = ::std::vec::Vec::new();
            #(elems.push(#ts);)*
            ::rex_type_system::types::ADTVariant {
                doc: #variant_docs,
                name: String::from(#variant_name),
                t: Some(Box::new(::rex_type_system::types::Type::Tuple(elems))),
                field_docs: None,
            }
        })
    }
}

fn fields_named_to_adt_variant(
    variant_docs: &proc_macro2::TokenStream,
    variant_name: &str,
    fields: &FieldsNamed,
) -> proc_macro2::TokenStream {
    let docs_and_fields = fields
        .named
        .iter()
        .map(|field| {
            let docs = docs_from_attrs(&field.attrs);
            let mut name = format!("{}", field.ident.as_ref().unwrap());
            let t = to_type(&field.ty);
            rename_field(&mut name, field);
            quote! {
                if let Some(d) = #docs {
                    docs.insert(String::from(#name), d);
                }
                fields.insert(String::from(#name), #t);
            }
        })
        .collect::<Vec<_>>();
    if docs_and_fields.len() == 0 {
        quote!(
            ::rex_type_system::types::ADTVariant {
                doc: #variant_docs,
                name: String::from(#variant_name),
                t: None,
                field_docs: None,
            }
        )
    } else {
        quote!({
            let mut docs = ::std::collections::BTreeMap::new();
            let mut fields = ::std::collections::BTreeMap::new();
            #(#docs_and_fields;)*
            ::rex_type_system::types::ADTVariant {
                doc: #variant_docs,
                name: String::from(#variant_name),
                t: Some(Box::new(::rex_type_system::types::Type::Dict(fields))),
                field_docs: if docs.len() > 0 { Some(docs) } else { None },
            }
        })
    }
}

fn docs_from_attrs(attrs: &[Attribute]) -> proc_macro2::TokenStream {
    let docs = attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("doc") {
                Some(attr.meta.clone())
            } else {
                None
            }
        })
        .filter_map(|meta| {
            if let syn::Meta::NameValue(value) = meta {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(lit_str),
                    ..
                }) = value.value
                {
                    let doc_str = lit_str.token().to_string();
                    if doc_str.starts_with("\" ") && doc_str.ends_with('\"') {
                        Some(doc_str[2..doc_str.len() - 1].to_string())
                    } else if doc_str.starts_with('\"') && doc_str.ends_with('\"') {
                        Some(doc_str[1..doc_str.len() - 1].to_string())
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .join("\n");

    if docs.is_empty() {
        quote! { None }
    } else {
        quote! { Some(String::from(#docs)) }
    }
}

fn rename_variant(variant_name: &mut String, variant: &syn::Variant) {
    for attr in &variant.attrs {
        let maybe_meta_tokens_iter = attr.meta.require_list().ok().and_then(|meta_list| {
            meta_list
                .path
                .segments
                .first()
                .filter(|serde_ident| serde_ident.ident == "serde")
                .map(|_| meta_list.tokens.clone().into_iter())
        });
        if let Some(mut meta_tokens_iter) = maybe_meta_tokens_iter {
            match (
                meta_tokens_iter.next().map(|t| t.to_string()).as_deref(),
                meta_tokens_iter.next().map(|t| t.to_string()).as_deref(),
                meta_tokens_iter.next().map(|t| t.to_string()),
            ) {
                (Some("rename"), Some("="), Some(variant_name_lit))
                    if variant_name_lit.starts_with('\"') && variant_name_lit.ends_with('\"') =>
                {
                    *variant_name = variant_name_lit[1..variant_name_lit.len() - 1].to_owned();
                    break;
                }
                _ => {}
            }
        };
    }
}

fn rename_field(field_name: &mut String, field: &syn::Field) {
    for attr in &field.attrs {
        let maybe_meta_tokens_iter = attr.meta.require_list().ok().and_then(|meta_list| {
            meta_list
                .path
                .segments
                .first()
                .filter(|serde_ident| serde_ident.ident == "serde")
                .map(|_| meta_list.tokens.clone().into_iter())
        });
        if let Some(mut meta_tokens_iter) = maybe_meta_tokens_iter {
            match (
                meta_tokens_iter.next().map(|t| t.to_string()).as_deref(),
                meta_tokens_iter.next().map(|t| t.to_string()).as_deref(),
                meta_tokens_iter.next().map(|t| t.to_string()),
            ) {
                (Some("rename"), Some("="), Some(field_name_lit))
                    if field_name_lit.starts_with('\"') && field_name_lit.ends_with('\"') =>
                {
                    *field_name = field_name_lit[1..field_name_lit.len() - 1].to_owned();
                    break;
                }
                _ => {}
            }
        };
    }
}

fn to_type(ty: &Type) -> proc_macro2::TokenStream {
    match ty {
        Type::Path(type_path) if type_path.qself.is_none() => {
            let ident = &type_path.path.segments.last().unwrap().ident;
            let inner_types = &type_path.path.segments.last().unwrap().arguments;
            quote!(<#ident #inner_types as ::rex_type_system::types::ToType>::to_type())
        }
        Type::Tuple(tuple) => {
            let inner_types = tuple.elems.iter().map(to_type);
            quote!(::rex_type_system::types::Type::Tuple(
                vec![#(#inner_types,)*]
            ))
        }
        _ => panic!("Unsupported type"),
    }
}
