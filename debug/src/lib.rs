extern crate proc_macro;

use proc_macro::TokenStream;
use quote::*;
use syn::{self, parse_macro_input, parse_quote, DeriveInput};
use syn::export::TokenStream2;

// Extracts name-value pair attribute from "above" field or `None` if none
// Relevant to: tests/03-custom-format.rs
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

// Adds trait bounds to input token stream's generics
fn add_trait_bounds(_add_debug_bound: bool, mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            // @todo check for `PhantomData...` here, if found, don't push `Debug` bound
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

fn is_phantom_data_type(p_seg: &syn::PathSegment) -> bool {
    if p_seg.ident.to_string().as_str().eq("PhantomData") {
        return true;
    }
    false
}

// Relevant to: tests/05-phantom-data.rs
fn is_phantom_data_with_type_param(ty_path: &syn::TypePath, ty_param: &syn::TypeParam) -> bool {
    let mut ty_param_used_count = 0;
    let mut phantom_count = 0;
    for ps in &ty_path.path.segments {
        if is_phantom_data_type(ps) {
            phantom_count += 1;
        }
        match &ps.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                for arg in &args.args {
                    if let syn::GenericArgument::Type(syn::Type::Path(arg_ty_path)) = arg {
                        for p_seg in &arg_ty_path.path.segments {
                            if ty_param.ident.eq(&p_seg.ident) {
                                eprintln!("{:}\n{:}", &ty_param.ident, &p_seg.ident);
                                ty_param_used_count += 1;
                            }
                        }
                    }
                }
            }
            _ => {
                phantom_count -= 1;
                continue;
            }
        }
    }
    eprintln!("arg count: {:}", &ty_param_used_count);
    phantom_count > 0 && ty_param_used_count > 0
}

fn generics_only_appear_in_phantom_data(generics: &syn::Generics, ty_path: &syn::TypePath) -> bool {
    let mut results: Vec<bool> = vec![];
    for param in &generics.params {
        if let syn::GenericParam::Type(ty_param) = param {
            results.push(is_phantom_data_with_type_param(ty_path, ty_param));
        }
    }
    results.into_iter().filter(|b| b == &true).count() == 1
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);
    let _struct_ident = &_ast.ident;
    let _element = match &_ast.data {
        syn::Data::Enum(_) => panic!("`CustomDebug` can only be derived for `structs`."),
        syn::Data::Union(_) => panic!("`CustomDebug` can only be derived for `structs`."),
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
    let mut _generics_appear_only_in_phantom_data: bool = false;
    let mut _generic_ty_in_phantom_data_checks: Vec<bool> = vec![];
    let mut _generic_appearnces_count = 0;
    for _field in _fields_iter.clone() {
        // Check if field has passed in generics in `PhantomData<...>`
        if let syn::Type::Path(ty_path) = &_field.ty {
            if generics_only_appear_in_phantom_data(&_ast.generics, ty_path) {
                _generic_appearnces_count += 1;
            }
        }

        let _field_ident = match &_field.ident {
            Some(_field_ident) => _field_ident,
            _ => continue
        };
        let _field_ident_str = format_ident!("{}", _field_ident);

        // Extract name value pair - tests/03-custom-format.rs
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

    let _generics = add_trait_bounds(_generic_appearnces_count <= 1, _ast.generics);
    let (impl_generics, ty_generics, where_clause) = &_generics.split_for_impl();
    let _expanded = quote! {
        impl #impl_generics std::fmt::Debug for #_struct_ident #ty_generics #where_clause {
          fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            println!(#_fmt_str, #(#_extract_parts,)*);
            write!(f, #_fmt_str, #(#_extract_parts,)*)
          }
        }
    };

    _expanded.into()
}
