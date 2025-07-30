use proc_macro::TokenStream;
use proc_macro2::{Span, TokenTree};
use quote::quote;
use syn::{
    parse_str, spanned::Spanned, Attribute, Data, DeriveInput, Error, Expr, ExprLit, ExprUnary,
    Fields, FieldsNamed, FieldsUnnamed, Ident, Lit, Meta, Type, UnOp, Variant,
};

#[derive(Debug)]
struct DeriveOptions {
    name: String,
}

impl DeriveOptions {
    fn from_derive_input(ast: &DeriveInput) -> Result<DeriveOptions, Error> {
        for attr in ast.attrs.iter() {
            let Meta::List(meta_list) = &attr.meta else {
                continue;
            };
            if !meta_list.path.is_ident("rex") {
                continue;
            }

            let mut token_iter = meta_list.tokens.clone().into_iter();
            match token_iter.next() {
                Some(TokenTree::Ident(ident)) if ident == "name" => (),
                Some(t) => return Err(Error::new(t.span(), "Expected \"name\"")),
                None => return Err(Error::new(meta_list.span(), "Expected \"name\"")),
            }

            match token_iter.next() {
                Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => (),
                Some(t) => return Err(Error::new(t.span(), "Expected '='")),
                None => return Err(Error::new(meta_list.span(), "Expected '='")),
            }

            let name: String = match token_iter.next() {
                Some(TokenTree::Literal(literal)) => {
                    match parse_str::<Lit>(&literal.to_string())? {
                        Lit::Str(lit_str) => lit_str.value(),
                        _ => return Err(Error::new(literal.span(), "Expected a string")),
                    }
                }
                Some(t) => return Err(Error::new(t.span(), "Expected a string")),
                None => return Err(Error::new(meta_list.span(), "Expected a string")),
            };

            return Ok(DeriveOptions { name });
        }

        Ok(DeriveOptions {
            name: ast.ident.to_string(),
        })
    }
}

#[proc_macro_derive(Rex, attributes(rex))]
pub fn derive_rex(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();

    let options = match DeriveOptions::from_derive_input(&ast) {
        Ok(a) => a,
        Err(e) => return e.into_compile_error().into(),
    };

    let mut expanded = TokenStream::new();
    expanded.extend(impl_to_type(&ast, &options));
    expanded.extend(impl_encode(&ast, &options));
    expanded.extend(impl_decode(&ast, &options));
    expanded
}

fn impl_to_type(ast: &DeriveInput, options: &DeriveOptions) -> TokenStream {
    let rust_ident = &ast.ident;
    let given_name = &options.name;
    let docs = docs_from_attrs(&ast.attrs);

    let r#impl = match &ast.data {
        Data::Struct(data) => match &data.fields {
            // Turns into an ADT with one dictionary variant
            Fields::Named(named) => {
                let adt_variant = fields_named_to_adt_variant(&docs, given_name, named);
                quote!(
                    ::rex::type_system::types::Type::ADT(::rex::type_system::types::ADT {
                        name: String::from(#given_name),
                        variants: vec![#adt_variant],
                        docs: #docs,
                    })
                )
            }
            // Turns into an ADT with one tuple variant (we drop the tuple if it
            // only has one element)
            Fields::Unnamed(unnamed) => {
                let adt_variant = fields_unnamed_to_adt_variant(&docs, given_name, unnamed);
                quote!(
                    ::rex::type_system::types::Type::ADT(::rex::type_system::types::ADT {
                        name: String::from(#given_name),
                        variants: vec![#adt_variant],
                        docs: #docs,
                    })
                )
            }
            _ => quote! {
                ::rex::type_system::types::Type::ADT(::rex::type_system::types::ADT {
                    name: String::from(#given_name),
                    variants: vec![],
                    docs: #docs,
                })
            },
        },
        Data::Enum(data) => {
            let variants = data.variants.iter().map(|variant| {
                let variant_docs = docs_from_attrs(&variant.attrs);
                let mut variant_name = format!("{}", variant.ident.clone());
                rename_variant(&mut variant_name, variant);
                let discriminant = match parse_int_discriminant(variant) {
                    Some(value) => {
                        quote!(Some(#value))
                    }
                    None => {
                        quote!(None)
                    }
                };

                match &variant.fields {
                    Fields::Unnamed(unnamed) => {
                        fields_unnamed_to_adt_variant(&variant_docs, &variant_name, unnamed)
                    }
                    Fields::Named(named) => {
                        fields_named_to_adt_variant(&variant_docs, &variant_name, named)
                    }
                    Fields::Unit => quote! {
                        ::rex::type_system::types::ADTVariant {
                            name: String::from(#variant_name),
                            t: None,
                            docs: #variant_docs,
                            t_docs: None,
                            discriminant: #discriminant,
                        }
                    },
                }
            });
            quote! {
                ::rex::type_system::types::Type::ADT(::rex::type_system::types::ADT {
                    name: String::from(#given_name),
                    variants: vec![#(#variants,)*],
                    docs: #docs,
                })
            }
        }
        _ => panic!("Rex can only be derived for structs and enums"),
    };

    let generics = &ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics ::rex::type_system::types::ToType for #rust_ident #ty_generics #where_clause {
            fn to_type() -> ::rex::type_system::types::Type {
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
                ::std::sync::Arc::new(#t)
            }
        })
        .collect::<Vec<_>>();
    if ts.is_empty() {
        quote!(
            ::rex::type_system::types::ADTVariant {
                name: String::from(#variant_name),
                t: None,
                docs: #variant_docs,
                t_docs: None,
                discriminant: None,
            }
        )
    } else if ts.len() == 1 {
        let t = &ts[0];
        quote!(
            ::rex::type_system::types::ADTVariant {
                name: String::from(#variant_name),
                t: Some(#t),
                docs: #variant_docs,
                t_docs: None,
                discriminant: None,
            }
        )
    } else {
        quote!({
            let mut elems = ::std::vec::Vec::new();
            #(elems.push(#ts);)*
            ::rex::type_system::types::ADTVariant {
                name: String::from(#variant_name),
                t: Some(::std::sync::Arc::new(::rex::type_system::types::Type::Tuple(elems))),
                docs: #variant_docs,
                t_docs: None,
                discriminant: None,
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
                fields.insert(String::from(#name), ::std::sync::Arc::new(#t));
            }
        })
        .collect::<Vec<_>>();
    if docs_and_fields.is_empty() {
        quote!(
            ::rex::type_system::types::ADTVariant {
                name: String::from(#variant_name),
                t: None,
                docs: #variant_docs,
                t_docs: None,
                discriminant: None,
            }
        )
    } else {
        quote!({
            let mut docs = ::std::collections::BTreeMap::new();
            let mut fields = ::std::collections::BTreeMap::new();
            #(#docs_and_fields;)*
            ::rex::type_system::types::ADTVariant {
                name: String::from(#variant_name),
                t: Some(::std::sync::Arc::new(::rex::type_system::types::Type::Dict(fields))),
                docs: #variant_docs,
                t_docs: if docs.len() > 0 { Some(docs) } else { None },
                discriminant: None,
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
        Type::Path(type_path) => {
            quote!(<#type_path as ::rex::type_system::types::ToType>::to_type())
        }
        Type::Tuple(tuple) => {
            let inner_types = tuple.elems.iter().map(to_type);
            quote!(::rex::type_system::types::Type::Tuple(
                vec![#(#inner_types,)*]
                    .into_iter()
                    .map(::std::sync::Arc::new)
                    .collect()
            ))
        }
        _ => panic!("Unsupported type"),
    }
}

fn impl_encode(ast: &DeriveInput, options: &DeriveOptions) -> TokenStream {
    let rust_ident = &ast.ident;
    let given_name = &options.name;
    let r#impl: proc_macro2::TokenStream = match &ast.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => {
                let items = named.named.iter().map(|field| {
                    let identifier = &field.ident.as_ref().unwrap();
                    quote!(
                        (
                            String::from(stringify!(#identifier)),
                            ::rex::engine::codec::Encode::try_encode(self.#identifier, span)?,
                        )
                    )
                });
                quote!(Ok(::std::sync::Arc::new(::rex::ast::expr::Expr::Named(
                    ::rex::lexer::span::Span::default(),
                    #given_name.to_string(),
                    Some(::std::sync::Arc::new(::rex::ast::expr::Expr::Dict(
                        ::rex::lexer::span::Span::default(),
                        ::std::collections::BTreeMap::from_iter(vec![#(#items,)*].into_iter())
                    )))
                ))))
            }
            Fields::Unnamed(unnamed) if unnamed.unnamed.len() == 1 => {
                quote!(Ok(::std::sync::Arc::new(::rex::ast::expr::Expr::Named(
                    ::rex::lexer::span::Span::default(),
                    #given_name.to_string(),
                    Some(::rex::engine::codec::Encode::try_encode(self.0, span)?)
                ))))
            }
            Fields::Unnamed(unnamed) => {
                let items = unnamed.unnamed.iter().enumerate().map(|(i, _)| {
                    let identifier = syn::Index::from(i);
                    quote!(::rex::engine::codec::Encode::try_encode(self.#identifier, span)?)
                });
                quote!(Ok(::std::sync::Arc::new(::rex::ast::expr::Expr::Named(
                    ::rex::lexer::span::Span::default(),
                    #given_name.to_string(),
                    Some(::std::sync::Arc::new(::rex::ast::expr::Expr::Tuple(
                        ::rex::lexer::span::Span::default(),
                        vec![#(#items,)*]
                    )))
                ))))
            }
            Fields::Unit => {
                quote!(Ok(::std::sync::Arc::new(::rex::ast::expr::Expr::Named(
                    ::rex::lexer::span::Span::default(),
                    #given_name.to_string(),
                    None
                ))))
            }
        },
        Data::Enum(data) => {
            let variants = data.variants.iter().map(|variant| {
                let mut variant_name = format!("{}", variant.ident.clone());
                rename_variant(&mut variant_name, variant);
                let variant_ident = &variant.ident;
                match &variant.fields {
                    Fields::Named(named) => {
                        let mut field_idents: Vec<proc_macro2::TokenStream> = Vec::new();
                        let mut field_exprs: Vec<proc_macro2::TokenStream> = Vec::new();
                        for field in named.named.iter() {
                            let identifier = &field.ident.as_ref().unwrap();
                            field_idents.push(quote!( #identifier ));
                            field_exprs.push(quote!((
                                String::from(stringify!(#identifier)),
                                ::rex::engine::codec::Encode::try_encode(#identifier, span)?,
                            )));
                        }
                        quote!(
                            Self::#variant_ident { #(#field_idents,)* } => {
                                Ok(::std::sync::Arc::new(::rex::ast::expr::Expr::Named(
                                    ::rex::lexer::span::Span::default(),
                                    String::from(#variant_name),
                                    Some(::std::sync::Arc::new(::rex::ast::expr::Expr::Dict(
                                        ::rex::lexer::span::Span::default(),
                                        ::std::collections::BTreeMap::from_iter(vec![
                                            #(#field_exprs,)*
                                            ].into_iter())
                                    )))
                                )))
                            }
                        )
                    }
                    Fields::Unnamed(unnamed) if unnamed.unnamed.len() == 1 => {
                        let identifier = Ident::new(&format!("x{}", 0), Span::call_site());
                        let field_ident = quote!( #identifier );
                        let field_expr = quote!(
                            ::rex::engine::codec::Encode::try_encode(#identifier, span)?);
                        quote!(
                            Self::#variant_ident ( #field_ident ) =>
                                Ok(::std::sync::Arc::new(::rex::ast::expr::Expr::Named(
                                    ::rex::lexer::span::Span::default(),
                                    String::from(#variant_name),
                                    Some(#field_expr)
                                )))
                        )
                    }
                    Fields::Unnamed(unnamed) => {
                        let mut field_idents: Vec<proc_macro2::TokenStream> = Vec::new();
                        let mut field_exprs: Vec<proc_macro2::TokenStream> = Vec::new();
                        for i in 0..unnamed.unnamed.len() {
                            let identifier = Ident::new(&format!("x{}", i), Span::call_site());
                            field_idents.push(quote!( #identifier ));
                            field_exprs.push(quote!(
                                ::rex::engine::codec::Encode::try_encode(#identifier, span)?));
                        }

                        quote!(
                            Self::#variant_ident ( #(#field_idents,)* ) =>
                                Ok(::std::sync::Arc::new(::rex::ast::expr::Expr::Named(
                                    ::rex::lexer::span::Span::default(),
                                    String::from(#variant_name),
                                    Some(::std::sync::Arc::new(::rex::ast::expr::Expr::Tuple(
                                        ::rex::lexer::span::Span::default(),
                                        vec![#(#field_exprs,)*]
                                    )))
                                )))
                        )
                    }
                    Fields::Unit => {
                        quote!(
                            Self::#variant_ident =>
                                Ok(::std::sync::Arc::new(::rex::ast::expr::Expr::Named(
                                    ::rex::lexer::span::Span::default(),
                                    String::from(#variant_name),
                                    None
                                )))
                        )
                    }
                }
            });
            quote! {
                match self {
                    #(#variants,)*
                }
            }
        }
        Data::Union(_) => {
            panic!("Rex can only be derived for structs and enums")
        }
    };

    let generics = &ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics ::rex::engine::codec::Encode for #rust_ident #ty_generics #where_clause {
            fn try_encode(
                self,
                span: ::rex::lexer::span::Span,
            ) -> Result<::std::sync::Arc<::rex::ast::expr::Expr>, ::rex::engine::error::Error> {
                #r#impl
            }
        }
    };

    TokenStream::from(expanded)
}

fn impl_decode(ast: &DeriveInput, options: &DeriveOptions) -> TokenStream {
    let rust_ident = &ast.ident;
    let given_name = &options.name;
    let r#impl: proc_macro2::TokenStream = match &ast.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => {
                let fields = named.named.iter().map(|field| {
                    let field_ident = &field.ident.as_ref().unwrap();
                    quote!( #field_ident: ::rex::engine::codec::Decode::try_decode(
                            entries.get(stringify!(#field_ident))
                                .ok_or_else(||
                                    ::rex::engine::error::Error::ExpectedTypeGotValue {
                                        expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                        got: v.clone(),
                                        trace: ::std::default::Default::default(),
                                    })?
                            )? )
                });
                quote!(
                    match &**v {
                        ::rex::ast::expr::Expr::Named(_, n, Some(inner)) if n == #given_name => {
                            match &**inner {
                                ::rex::ast::expr::Expr::Dict(_, entries) => {
                                    Ok(#rust_ident {
                                        #(#fields,)*
                                    })
                                }
                                _ => {
                                    Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                                        expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                        got: v.clone(),
                                        trace: ::std::default::Default::default(),
                                    })
                                }
                            }
                        }
                        _ => {
                            Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                                expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                got: v.clone(),
                                trace: ::std::default::Default::default(),
                            })
                        }
                    }
                )
            }
            Fields::Unnamed(unnamed) if unnamed.unnamed.len() == 1 => {
                quote!(
                    match &**v {
                        ::rex::ast::expr::Expr::Named(_, n, Some(inner)) if n == #given_name => {
                            Ok(#rust_ident (::rex::engine::codec::Decode::try_decode(inner)?))
                        },
                        _ => {
                            Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                                expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                got: v.clone(),
                                trace: ::std::default::Default::default(),
                            })
                        }
                    }
                )
            }
            Fields::Unnamed(unnamed) => {
                let items_len = unnamed.unnamed.len();
                let fields = (0..items_len)
                    .map(|i| quote!( ::rex::engine::codec::Decode::try_decode(&items[#i])?));

                quote!(
                    match &**v {
                        ::rex::ast::expr::Expr::Named(_, n, Some(inner)) if n == #given_name => {
                            match &**inner {
                                ::rex::ast::expr::Expr::Tuple(_, items) if items.len() == #items_len => {
                                    Ok(#rust_ident (
                                        #(#fields,)*
                                    ))
                                }
                                _ => {
                                    Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                                        expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                        got: v.clone(),
                                        trace: ::std::default::Default::default(),
                                    })
                                }
                            }
                        },
                        _ => {
                            Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                                expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                got: v.clone(),
                                trace: ::std::default::Default::default(),
                            })
                        }
                    }
                )
            }
            Fields::Unit => {
                quote!(
                    match &**v {
                        ::rex::ast::expr::Expr::Named(_, n, None) if n == #given_name => {
                            Ok(#rust_ident)
                        }
                        _ => {
                            Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                                expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                got: v.clone(),
                                trace: ::std::default::Default::default(),
                            })
                        }
                    }
                )
            }
        },
        Data::Enum(data) => {
            let variants = data.variants.iter().map(|variant| {
                let mut variant_name = format!("{}", variant.ident.clone());
                rename_variant(&mut variant_name, variant);
                let variant_ident = &variant.ident;
                match &variant.fields {
                    Fields::Named(named) => {
                        let fields = named.named.iter().map(|field| {
                            let field_ident = &field.ident.as_ref().unwrap();
                            quote!( #field_ident: ::rex::engine::codec::Decode::try_decode(
                                entries.get(stringify!(#field_ident))
                                    .ok_or_else(||
                                        ::rex::engine::error::Error::ExpectedTypeGotValue {
                                            expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                            got: v.clone(),
                                            trace: ::std::default::Default::default(),
                                        })?
                                )? )
                        });

                        quote!(
                            ::rex::ast::expr::Expr::Named(_, n, Some(inner))
                            if n == #variant_name => {
                                match &**inner {
                                    ::rex::ast::expr::Expr::Dict(_, entries) => {
                                        Ok(Self::#variant_ident { #(#fields,)* })
                                    }
                                    _ => {
                                        Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                                            expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                            got: v.clone(),
                                            trace: ::std::default::Default::default(),
                                        })
                                    }
                                }
                            }
                        )
                    }
                    Fields::Unnamed(unnamed) if unnamed.unnamed.len() == 1 => {
                        quote!(
                            ::rex::ast::expr::Expr::Named(_, n, Some(inner))
                            if n == #variant_name => {
                                Ok(Self::#variant_ident (
                                    ::rex::engine::codec::Decode::try_decode(inner)?
                                ))
                            }
                        )
                    }
                    Fields::Unnamed(unnamed) => {
                        let items_len = unnamed.unnamed.len();
                        let fields = (0..items_len).map(
                            |i| quote!( ::rex::engine::codec::Decode::try_decode(&items[#i])?),
                        );

                        quote!(
                            ::rex::ast::expr::Expr::Named(_, n, Some(inner))
                            if n == #variant_name => {
                                match &**inner {
                                    ::rex::ast::expr::Expr::Tuple(_, items)
                                    if items.len() == #items_len => {
                                        Ok(Self::#variant_ident (
                                            #(#fields,)*
                                        ))
                                    }
                                    _ => {
                                        Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                                            expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                                            got: v.clone(),
                                            trace: ::std::default::Default::default(),
                                        })
                                    }
                                }
                            }
                        )
                    }
                    Fields::Unit => {
                        quote!(
                            ::rex::ast::expr::Expr::Named(_, n, None)
                            if n == #variant_name => {
                                Ok(Self::#variant_ident)
                            }
                        )
                    }
                }
            });
            quote! {
                match &**v {
                    #(#variants,)*
                    _ => {
                        Err(::rex::engine::error::Error::ExpectedTypeGotValue {
                            expected: ::std::sync::Arc::new(<Self as ::rex::type_system::types::ToType>::to_type()),
                            got: v.clone(),
                            trace: ::std::default::Default::default(),
                        })
                    }
                }
            }
        }
        Data::Union(_) => {
            panic!("Rex can only be derived for structs and enums")
        }
    };

    let generics = &ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics ::rex::engine::codec::Decode for #rust_ident #ty_generics #where_clause {
            fn try_decode(
                v: &::std::sync::Arc<::rex::ast::expr::Expr>,
            ) -> Result<Self, ::rex::engine::error::Error> {
                #r#impl
            }
        }
    };

    TokenStream::from(expanded)
}

fn parse_int_discriminant(variant: &Variant) -> Option<i64> {
    match variant.discriminant {
        Some((_, ref expr)) => parse_int_literal(expr),
        None => None,
    }
}

fn parse_int_literal(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Int(ref i),
            ..
        }) => i.base10_parse::<i64>().ok(),
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            ref expr,
            ..
        }) => match &**expr {
            Expr::Lit(ExprLit {
                lit: Lit::Int(ref i),
                ..
            }) => i.base10_parse::<i64>().ok().map(|v| -v),
            _ => None,
        },
        _ => None,
    }
}
