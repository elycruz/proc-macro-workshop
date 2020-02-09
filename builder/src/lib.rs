extern crate proc_macro;
extern crate syn;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, DataStruct, DataEnum, Data, Fields, Field};
use quote::quote;
use syn::export::{Span, TokenStream2};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);
    let (_bfields, _bmethods, _bextracts, _bdefaults) = match &_ast.data {
        Data::Enum(_) => panic!("`Builder` can only be derived for `structs`."),
        Data::Union(_) => panic!("`Builder` can only be derived for `structs`."),
        Data::Struct(element) => {
            let (fields, methods) = element.fields.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                (quote! {
                    #name: std::option::Option<#ty>
                },
                 quote! {
                    pub fn #name (&mut self, x: #ty) -> &mut Self {
                        self.#name = Some(x);
                        self
                    }
                })
            })
                .unzip() as (Vec<TokenStream2>, Vec<TokenStream2>);

            let (extracts, defaults) = element.fields.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                (quote! {
                    #name: self.#name.clone().ok_or("Empty not allowed").unwrap()
                },
                 quote! {
                    #name: None
                })
            })
                .unzip() as (Vec<TokenStream2>, Vec<TokenStream2>);

            (fields, methods, extracts, defaults)
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
