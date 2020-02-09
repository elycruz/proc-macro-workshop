extern crate proc_macro;
extern crate syn;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, GenericArgument};
use quote::quote;
use syn::export::{Span, TokenStream2};

fn is_option_type_path(tp: &syn::TypePath) -> bool {
    tp.path.segments.len() > 0 && tp.path.segments[0].ident == "Option" // @todo check for std::option::Option here
}

fn unwrap_option_ty(ty: &syn::Type) -> &syn::Type {
    match ty {
        syn::Type::Path(tp) => {
            if is_option_type_path(tp) {
                match &tp.path.segments[0].arguments {
                    syn::PathArguments::AngleBracketed(pa) =>
                        match &pa.args[0] {
                            GenericArgument::Type(t) => t,
                            _ => panic!("malformed `Option<T>` encountered")
                        },
                    _ => unreachable!()
                }
            } else {
                ty
            }
        }
        _ => ty,
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);
    let (_bfields, _bmethods, _bextracts, _bdefaults) = match &_ast.data {
        Data::Enum(_) => panic!("`Builder` can only be derived for `structs`."),
        Data::Union(_) => panic!("`Builder` can only be derived for `structs`."),
        Data::Struct(element) => {
            let mut _fields: Vec<TokenStream2> = vec![];
            let mut _methods: Vec<TokenStream2> = vec![];
            let mut _extracts: Vec<TokenStream2> = vec![];
            let mut _defaults: Vec<TokenStream2> = vec![];
            for field in element.fields.iter() {
                let name = &field.ident;
                let ty = &field.ty;
                let mut is_option_tp: bool = false;
                let resolved_ty = unwrap_option_ty(ty);
                match ty {
                    syn::Type::Path(tp) => {
                        is_option_tp = is_option_type_path(tp);
                        _fields.push(if is_option_tp {
                            quote! {
                                #name: #ty
                            }
                        } else {
                            quote! {
                                #name: std::option::Option<#ty>
                            }
                        });
                    }
                    _ => unreachable!()
                }
                _methods.push(quote! {
                    pub fn #name (&mut self, x: #resolved_ty) -> &mut Self {
                        self.#name = Some(x);
                        self
                    }
                });
                _extracts.push(if is_option_tp {
                    quote! {
                        #name: self.#name.clone()
                    }
                } else {
                    quote! {
                        #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is required.")).unwrap()
                    }
                });
                _defaults.push(quote! {
                    #name: None
                });
            }
            (_fields, _methods, _extracts, _defaults)
        }
    };
    let _nident = &_ast.ident;
    let _bname = format!("{}Builder", _nident);
    let _bident = syn::Ident::new(&_bname, Span::call_site());
    let _expanded = quote! {
        pub struct #_bident {
            #(#_bfields,)*
        }

        impl #_bident {
            #(#_bmethods)*

            pub fn build(&mut self) -> Result<#_nident, Box<dyn std::error::Error>> {
                Ok(#_nident {
                    #(#_bextracts,)*
                })
            }
        }

        impl #_nident {
            fn builder () -> #_bident {
                #_bident {
                    #(#_bdefaults,)*
                }
            }
        }
    };

    _expanded.into()
}
