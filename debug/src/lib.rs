extern crate proc_macro;

use proc_macro::TokenStream;
use quote::*;
use syn::{self, parse_macro_input, parse_quote, DeriveInput, Generics};
use syn::export::TokenStream2;
use std::any::Any;

// Extracts name-value pair attribute from above field or `None` if none
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
    // eprintln!("path segment: {:}", ps.to_token_stream());
    if p_seg.ident.to_string().as_str().eq("PhantomData") {
        return true;
    }
    false
}

// Relevant to: tests/05-phantom-data.rs
fn has_phantom_data_type_with_generics(typ: &syn::TypePath, generics: &syn::Generics) -> bool {
    for ps in &typ.path.segments {
        if !is_phantom_data_type(ps) {
            continue;
        }
        if let syn::PathArguments::AngleBracketed(g_args) = &ps.arguments {
            for g_arg in &g_args.args.into_iter() {
                match g_arg {
                    syn::GenericArgument::Type(syn::Type::Path(g_arg_ty_path)) => {
                        eprintln!("type path -> segment -> generics: {:#?}", g_arg_ty_path);
                    }
                    _ => continue
                }
            }
        }
    }
    false
}

fn generics_only_appear_in_phantom_data(generics: &syn::Generics, ty_path: &syn::TypePath) -> bool {
    for param in &generics.params {
        if let syn::GenericParam::Type(g_param) = param {
            if is_phantom_data_type_with_generic(ty_path, g_param) {
                return true;
            }
        }
    }
    false
}

fn field_uses_type_param(field: &syn::Field, type_param: &syn::TypeParam) -> bool {
    eprintln!("{:}", type_param.to_token_stream());
    if let syn::Type::Path(tp) = &field.ty {
        for ps in &tp.path.segments {
            eprintln!("path segment argument: {:}", &ps.arguments.to_token_stream());
        }
    }
    false
}

fn field_uses_generics(field: &syn::Field, generics: &syn::Generics) -> bool {
    if let syn::Type::Path(type_path) = &field.ty {
        for ps in &type_path.path.segments {
            for g_param in &generics.params {
                if let syn::GenericParam::Type(type_param) = g_param {
                    eprintln!("Generic param: {:}", type_param.to_token_stream());
                }
            }
            eprintln!("Field's type: {:}", ps.to_token_stream());
        }
    }
    false
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
    let mut _generic_ty_in_phantom_data: bool = false;
    let mut _generic_ty_in_phantom_data_checks: Vec<bool> = vec![];
    for _field in _fields_iter.clone() {
        // Check if field has passed in generics in `PhantomData<...>`
        if let syn::Type::Path(ty_path) = &_field.ty {
            generics_only_appear_in_phantom_data(&_ast.generics, ty_path);
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

    let _generics = add_trait_bounds(_generic_ty_in_phantom_data, _ast.generics);
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
