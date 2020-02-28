extern crate proc_macro;

use proc_macro::TokenStream;
use quote::*;
use syn::{self, parse_macro_input, DeriveInput};
use syn::export::TokenStream2;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);
    let _struct_ident = &_ast.ident;
    let _element = match &_ast.data {
        syn::Data::Enum(_) => panic!("`Builder` can only be derived for `structs`."),
        syn::Data::Union(_) => panic!("`Builder` can only be derived for `structs`."),
        syn::Data::Struct(_element) => _element
    };
    let _fields_iter = match &_element.fields {
        syn::Fields::Unnamed(_) => {
            panic!("named fields required")
        }
        syn::Fields::Unit => {
            panic!("unit not allowed")
        }
        syn::Fields::Named(syn::FieldsNamed{named, ..}) => named.iter()
    };

    let mut _fmt_parts: Vec<String> = vec![];
    let mut _extract_parts: Vec<TokenStream2> = vec![];
    for _field in _fields_iter.clone() {
        match &_field.ident {
            Some(_field_ident) => {
                let _field_ident_str = format_ident!("{}", _field_ident);
                _fmt_parts.push(format!("{}: {{}}", _field_ident_str));
                _extract_parts.push(quote!{format!("{:?}", self.#_field_ident)});
            }
            _ => ()
        }
    }

    let mut _fmt_str = format!(
        "{} {}",
        _struct_ident,
        vec![
            "{{".to_string(),
            _fmt_parts.join(", "),
            "}}".to_string()
        ].join(" "))
    ;

    let _expanded = quote! {
        impl std::fmt::Debug for #_struct_ident {
          fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            println!(#_fmt_str, #(#_extract_parts,)*);
            write!(f, #_fmt_str, #(#_extract_parts,)*)
          }
        }
    };

    _expanded.into()
}
