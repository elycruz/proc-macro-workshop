extern crate proc_macro;
extern crate syn;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);
    let _nident = &_ast.ident;
    let _bname = format!("{}Builder", _nident);
    let _bident = syn::Ident::new(&_bname, _nident.span());

//    eprintln!("{:#?}", &_ast);

    let _expanded = quote! {
        pub struct #_bident {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }

        impl #_bident {
            pub fn executable(&mut self, s: String) -> &mut Self {
                self.executable = Some(s);
                self
            }
            pub fn args(&mut self, vs: Vec<String>) -> &mut Self {
                self.args = Some(vs);
                self
            }
            pub fn env(&mut self, vs: Vec<String>) -> &mut Self {
                self.env = Some(vs);
                self
            }
            pub fn current_dir(&mut self, s: String) -> &mut Self {
                self.current_dir = Some(s);
                self
            }

            pub fn build(&mut self) -> Result<#_nident, Box<dyn std::error::Error>> {
                Ok(#_nident {
                    executable: self.executable.clone().ok_or("executable is empty").unwrap(),
                    args: self.args.clone().ok_or("args is empty").unwrap(),
                    env: self.env.clone().ok_or("env is empty").unwrap(),
                    current_dir: self.current_dir.clone().ok_or("current_dir is empty").unwrap(),
                })
            }
        }

        impl #_nident {
            fn builder () -> #_bident {
                #_bident {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }
    };

    _expanded.into()
}
