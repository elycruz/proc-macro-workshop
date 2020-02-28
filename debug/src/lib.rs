extern crate proc_macro;

use proc_macro::TokenStream;
use quote::*;
use syn::{self, parse_macro_input, DeriveInput};
use syn::export::TokenStream2;

fn extract_debug_name_value_pair(_field: &syn::Field) -> Result<Option<syn::MetaNameValue>, syn::Error> {
    for attr in &_field.attrs {
        if !attr.path.is_ident("debug") {
            continue;
        }
        match attr.parse_meta() {
            Err(e) => {
                return Err(e);
            }
            Ok(meta) => match meta {
                syn::Meta::NameValue(_name_value_pair) => {
                    return Ok(Some(_name_value_pair));
                }
                _ => continue
            }
        }
    }
    Ok(None)
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
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
        syn::Fields::Named(syn::FieldsNamed { named, .. }) => named.iter()
    };

    let mut _fmt_parts: Vec<String> = vec![];
    let mut _extract_parts: Vec<TokenStream2> = vec![];
    for _field in _fields_iter.clone() {
        let _field_ident = match &_field.ident {
            Some(_field_ident) => _field_ident,
            _ => continue
        };
        let _field_ident_str = format_ident!("{}", _field_ident);
        let _extract = match extract_debug_name_value_pair(_field) {
            Ok(Some(_name_value_pair)) => {
                let lit = _name_value_pair.lit;
                quote! {std::fmt::format(format_args!(#lit, self.#_field_ident))}
            }
            Ok(None) => quote! {format!("{:?}", self.#_field_ident)},
            Err(e) => return e.to_compile_error().into()
        };
        _fmt_parts.push(format!("{}: {{}}", _field_ident_str));
        _extract_parts.push(_extract);
    }

    let mut _fmt_str = format!(
        "{} {}",
        _struct_ident,
        vec![
            "{{".to_string(),
            _fmt_parts.join(", "),
            "}}".to_string()
        ]
            .join(" "));

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
