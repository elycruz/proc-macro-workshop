extern crate proc_macro;
extern crate syn;

use syn::{parse_macro_input, DeriveInput};
use quote::quote;
use proc_macro2::{Span, TokenStream, Ident, TokenTree};
use syn::export::ToTokens;

const MALFORMED_ATTR_MSG: &str = "expected `builder(each = \"...\")`";

fn is_option_type_path(tp: &syn::TypePath) -> bool {
    tp.path.segments.len() > 0 && (
        tp.path.segments[0].ident == "Option" ||
            tp.path.segments[0].ident == "std::option::Option"
    )
}

fn is_type_path_match(pred: impl Fn(&syn::TypePath) -> bool, ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(tp) => pred(tp),
        _ => false
    }
}

fn always_true_on_tp(_: &syn::TypePath) -> bool {
    true
}

fn unwrap_ty_on_tp(pred: impl Fn(&syn::TypePath) -> bool, ty: &syn::Type) -> std::result::Result<&syn::Type, &'static str> {
    match ty {
        syn::Type::Path(tp) => {
            if !pred(tp) {
                return std::result::Result::Ok(ty);
            }
            match &tp.path.segments[0].arguments {
                syn::PathArguments::AngleBracketed(pa) =>
                    match &pa.args[0] {
                        syn::GenericArgument::Type(t) => std::result::Result::Ok(t),
                        _ => std::result::Result::Err("malformed `U<V>` encountered")
                    },
                _ => unreachable!()
            }
        }
        _ => std::result::Result::Ok(ty)
    }
}

fn parse_attrs(f: &syn::Field, method_quotes: &mut Vec<TokenStream>) -> std::result::Result<std::option::Option<Ident>, syn::Error> {
    let attrs = &f.attrs;
    let name = match &f.ident {
        std::option::Option::Some(i) => i.clone(),
        _ => unreachable!()
    };
    let mut attr_name_ident = std::option::Option::None;
    let ty = &f.ty;
    if attrs.len() == 0 {
        return std::result::Result::Ok(std::option::Option::None);
    }
    for attr in attrs {
        // Only match 'outer' attribute types
        // @see https://docs.rs/syn/1.0.14/syn/struct.Attribute.html
        match attr.style {
            syn::AttrStyle::Inner(_) => continue,
            _ => ()
        }
        let attr_stream =
            if let std::option::Option::Some(proc_macro2::TokenTree::Group(_group)) =
            attr.clone().into_token_stream().into_iter().nth(1) {
                _group.stream()
            } else {
                attr.clone().into_token_stream()
            };
        if let std::option::Option::Some(ps) = attr.path.segments.first() {
            if ps.ident != "builder" {
                continue;
            }
            if attr.tokens.is_empty() {
                return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG));
            }
        }

        // Get the `Group` value
        if let std::option::Option::Some(proc_macro2::TokenTree::Group(g)) = attr.tokens.clone().into_iter().next() {
            let mut ts = g.clone().stream().into_iter();

            // 'each' token
            match ts.next() {
                std::option::Option::Some(tt) => if &tt.to_string() != "each" {
                    return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG));
                },
                _ => return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG))
            }

            // '=' token
            match ts.next() {
                std::option::Option::Some(tt) =>
                    if &tt.to_string() != "=" {
                        return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG));
                    },
                _ => return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG))
            }

            // If we 'method_name' literal convert and use it
            if let std::option::Option::Some(TokenTree::Literal(fn_name_lit)) = ts.next() {
                let fn_name = match syn::Lit::new(fn_name_lit) {
                    syn::Lit::Str(ls) => ls.value(),
                    _ => return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG))
                };
                let ident = Ident::new(&fn_name, Span::call_site());
                let inner_ty = match unwrap_ty_on_tp(always_true_on_tp, ty) {
                    std::result::Result::Ok(_type) => _type,
                    _ => return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG))
                };
                // if field and requested method name are the same, bail
                if ident.eq(&name) {
                    continue;
                }
                attr_name_ident = std::option::Option::Some(name.clone());
                method_quotes.push(quote! {
                    pub fn #ident (&mut self, x: #inner_ty) -> &mut Self {
                        match &mut self.#name {
                            std::option::Option::Some(xs) => {
                                xs.push(x);
                            },
                            _ => {
                                self.#name = std::option::Option::Some(vec![x]);
                            }
                        }
                        self
                    }
                });
            } else {
                return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG));
            }
        } else {
            return std::result::Result::Err(syn::Error::new_spanned(attr_stream, MALFORMED_ATTR_MSG));
        }
    }
    std::result::Result::Ok(attr_name_ident)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);
    let (_bfields, _bmethods, _bextracts) = match &_ast.data {
        syn::Data::Enum(_) => panic!("`Builder` can only be derived for `structs`."),
        syn::Data::Union(_) => panic!("`Builder` can only be derived for `structs`."),
        syn::Data::Struct(element) => {
            let mut _fields: Vec<TokenStream> = vec![];
            let mut _methods: Vec<TokenStream> = vec![];
            let mut _extracts: Vec<TokenStream> = vec![];
            for field in element.fields.iter() {
                let name = match &field.ident {
                    std::option::Option::Some(i) => i,
                    _ => unreachable!()
                };
                let ty = &field.ty;
                let resolved_ty = match unwrap_ty_on_tp(is_option_type_path, ty) {
                    std::result::Result::Ok(_type) => _type,
                    _ => unreachable!()
                };
                let is_option_tp: bool = is_type_path_match(is_option_type_path, ty);
                _fields.push(if is_option_tp {
                    quote! {
                        #name: #ty
                    }
                } else {
                    quote! {
                        #name: std::option::Option<#ty>
                    }
                });
                let attrs_parse_result: std::result::Result<(), syn::Error> = match parse_attrs(field, &mut _methods) {
                    std::result::Result::Ok(attr_meth_name) => {
                        if attr_meth_name.is_none() || !(&attr_meth_name.unwrap()).eq(name) {
                            _methods.push(quote! {
                                pub fn #name (&mut self, x: #resolved_ty) -> &mut Self {
                                    self.#name = std::option::Option::Some(x);
                                    self
                                }
                            });
                        }
                        _extracts.push(if is_option_tp {
                            quote! {
                                #name: self.#name.clone()
                            }
                        } else {
                            quote! {
                                    #name: if self.#name.is_none() {
                                        std::default::Default::default()
                                    } else {
                                        self.#name.clone().unwrap()
                                    }
                                }
                        });
                        std::result::Result::Ok(())
                    }
                    std::result::Result::Err(e) => std::result::Result::Err(e)
                };

                if let std::result::Result::Err(e) = attrs_parse_result {
                    return e.to_compile_error().into();
                }
            }
            (_fields, _methods, _extracts)
        }
    };
    let _nident = &_ast.ident;
    let _bname = format!("{}Builder", _nident);
    let _bident = syn::Ident::new(&_bname, Span::call_site());
    let _expanded = quote! {
        #[derive(Default)]
        pub struct #_bident {
            #(#_bfields,)*
        }

        impl #_bident {
            #(#_bmethods)*

            pub fn build(&mut self) -> std::result::Result<#_nident, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#_nident {
                    #(#_bextracts,)*
                })
            }
        }

        impl #_nident {
            fn builder () -> #_bident {
                std::default::Default::default()
            }
        }
    };

    _expanded.into()
}
