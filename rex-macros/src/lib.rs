use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Type, Variant,
};

#[proc_macro_derive(AbstractDataType)]
pub fn derive_adt(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let ident = &ast.ident;

    let t = match &ast.data {
        Data::Struct(data) => expand_derive_adt_for_struct(&ast, data),
        Data::Enum(data) => expand_derive_adt_for_enum(&ast, data),
        _ => panic!("filter can only be implemented for structs and enums"),
    };

    let generics = ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl  #impl_generics ::rex_ast::TypeInfo for #ident #ty_generics #where_clause {
            fn t() -> ::ouroboros::Type {
                #t
            }
        }
    };

    TokenStream::from(expanded)
}

fn expand_derive_adt_for_struct(ast: &DeriveInput, data: &DataStruct) -> proc_macro2::TokenStream {
    match &data.fields {
        Fields::Named(named_fields) => expand_derive_adt_for_record_struct(ast, named_fields),
        Fields::Unnamed(unnamed_fields) => expand_derive_adt_for_tuple_struct(ast, unnamed_fields),
        Fields::Unit => expand_derive_adt_for_unit_struct(ast),
    }
}

fn expand_derive_adt_for_record_struct(
    ast: &DeriveInput,
    fields: &FieldsNamed,
) -> proc_macro2::TokenStream {
    let ident = &ast.ident;
    let field_decls = fields.named.iter().map(|field| {
        let field_ident = &field.ident;
        let field_t = expand_t(&field.ty);
        quote! {
            fields.insert(stringify!(#field_ident).to_string(), #field_t);
        }
    });
    quote! {
        let mut fields = ::std::collections::BTreeMap::new();
        #(#field_decls;)*
        ::rex_ast::Type::ADT(::rex_ast::ADT {
            name: stringify!(#ident).to_string(),
            generics: vec![],
            variants: vec![::rex_ast::ADTVariant {
                name: stringify!(#ident).to_string(),
                fields: Some(::rex_ast::ADTVariantFields::Named(fields)),
            }],
        })
    }
}

fn expand_derive_adt_for_tuple_struct(
    ast: &DeriveInput,
    fields: &FieldsUnnamed,
) -> proc_macro2::TokenStream {
    let ident = &ast.ident;
    let fields = fields.unnamed.iter().map(|field| {
        let field_t = expand_t(&field.ty);
        quote! {
            #field_t
        }
    });
    quote! {
        ::rex_ast::Type::ADT(::rex_ast::ADT {
            name: stringify!(#ident).to_string(),
            generics: vec![],
            variants: vec![::rex_ast::ADTVariant {
                name: stringify!(#ident).to_string(),
                fields: Some(::rex_ast::ADTVariantFields::Unnamed(vec![#(#fields,)*])),
            }],
        })
    }
}

fn expand_derive_adt_for_unit_struct(ast: &DeriveInput) -> proc_macro2::TokenStream {
    let ident = &ast.ident;
    quote! {
        ::rex_ast::Type::ADT(::rex_ast::ADT {
            name: stringify!(#ident).to_string(),
            generics: vec![],
            variants: vec![::rex_ast::ADTVariant {
                name: stringify!(#ident).to_string(),
                fields: None,
            }],
        })
    }
}

fn expand_derive_adt_for_enum(ast: &DeriveInput, data: &DataEnum) -> proc_macro2::TokenStream {
    let ident = &ast.ident;
    let variants = data.variants.iter().map(|variant| {
        let mut variant_name = format!("{}", &variant.ident);
        fix_variant_name(&mut variant_name, variant);

        match &variant.fields {
            Fields::Named(fields) => {
                let field_decls = fields.named.iter().map(|field| {
                    let field_ident = &field.ident;
                    let field_t = expand_t(&field.ty);
                    quote! {
                        fields.insert(stringify!(#field_ident).to_string(), #field_t);
                    }
                });
                quote! {
                    let mut fields = ::std::collections::BTreeMap::new();
                    #(#field_decls;)*
                    ::rex_ast::ADTVariant {
                        name: #variant_name.to_string(),
                        fields: Some(::rex_ast::ADTVariantFields::Named(fields)),
                    }
                }
            }
            Fields::Unnamed(fields) => {
                let fields = fields.unnamed.iter().map(|field| {
                    let field_t = expand_t(&field.ty);
                    quote! {
                        #field_t
                    }
                });
                quote! {
                    ::rex_ast::ADTVariant {
                        name: #variant_name.to_string(),
                        fields: Some(::rex_ast::ADTVariantFields::Unnamed(vec![#(#fields,)*])),
                    }
                }
            }
            Fields::Unit => quote! {
                ::rex_ast::ADTVariant {
                    name: #variant_name.to_string(),
                    fields: None,
                }
            },
        }
    });
    quote! {
        ::rex_ast::Type::ADT(::rex_ast::ADT {
            name: stringify!(#ident).to_string(),
            generics: vec![],
            variants: vec![#(#variants,)*],
        })
    }
}

fn expand_t(ty: &Type) -> proc_macro2::TokenStream {
    match ty {
        Type::Path(type_path) if type_path.qself.is_none() => {
            let ident = &type_path.path.segments.last().unwrap().ident;
            let inner_types = &type_path.path.segments.last().unwrap().arguments;
            quote!(<#ident #inner_types as ::rex_ast::TypeInfo>::t())
        }
        Type::Tuple(tuple) => {
            let inner_types = tuple.elems.iter().map(expand_t);
            quote! { ::rex_ast::Type::Tuple(vec![#(#inner_types,)*]) }
        }
        _ => panic!("Unsupported type"),
    }
}

fn fix_variant_name(variant_name: &mut String, variant: &Variant) {
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
