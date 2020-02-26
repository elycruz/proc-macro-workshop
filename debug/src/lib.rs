extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{self, parse_macro_input, DeriveInput};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);
    let _struct_ident = &_ast.ident;

    match &_ast.data {
        syn::Data::Enum(_) => panic!("`Builder` can only be derived for `structs`."),
        syn::Data::Union(_) => panic!("`Builder` can only be derived for `structs`."),
        syn::Data::Struct(_element) => {
            eprintln!("{:#?}", _ast.ident);

            match &_element.fields {
                syn::Fields::Unnamed(_) => {
                    panic!("Only named fields allowed for structs")
                }
                syn::Fields::Unit => {
                    panic!("Only named fields allowed for structs")
                }
                syn::Fields::Named(fields) => {
                    let fields =  fields.named.iter();
                    for field in fields {
                        eprintln!("{:#?}", &field.ident);
                    }
                }
            } // fields match
        } // data struct match
    }

    let _expanded = quote! {
        impl std::fmt::Debug for #_struct_ident {
          fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "#_struct_ident") // {{ x: {}, y: {} }}", self.x, self.y)
          }
        }
    };

    _expanded.into()
}
