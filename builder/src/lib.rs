extern crate proc_macro;
extern crate syn;

use syn::{parse_macro_input, DeriveInput};
use quote::quote;
use proc_macro2::{Span, TokenStream, Literal, Ident, TokenTree};

const MALFORMED_ATTR_MSG: &str = "Malformed builder attribute; Expected format `each = \"method-name-to-create\"`;";

fn is_option_type_path(tp: &syn::TypePath) -> bool {
    tp.path.segments.len() > 0 && tp.path.segments[0].ident == "Option" // @todo check for std::option::Option here
}

fn is_type_path_match(pred: impl Fn(&syn::TypePath) -> bool, ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(tp) => pred(tp),
        _ => false
    }
}

fn always_true(tp: &syn::TypePath) -> bool {
    true
}

fn unwrap_ty_on_tp(pred: impl Fn(&syn::TypePath) -> bool, ty: &syn::Type) -> &syn::Type {
    match ty {
        syn::Type::Path(tp) => {
            if !pred(tp) {
                return ty;
            }
            match &tp.path.segments[0].arguments {
                syn::PathArguments::AngleBracketed(pa) =>
                    match &pa.args[0] {
                        syn::GenericArgument::Type(t) => t,
                        _ => panic!("malformed `U<V>` encountered")
                    },
                _ => unreachable!()
            }
        }
        _ => ty
    }
}

fn resolve_attrs_methods(f: &syn::Field, method_quotes: &mut Vec<TokenStream>) {
    let attrs = &f.attrs;
    let name = &f.ident;
    let ty = &f.ty;
    if attrs.len() == 0 {
        return;
    }
    for attr in attrs {
        // Only match 'outer' attribute types
        // @see https://docs.rs/syn/1.0.14/syn/struct.Attribute.html
        match attr.style {
            syn::AttrStyle::Inner(_) => continue,
            _ => ()
        }

        if let Some(ps) = attr.path.segments.first() {
            if ps.ident != "builder" {
                continue;
            }
            if attr.tokens.is_empty() {
                panic!("The `builder` attribute requires a group value;  E.g., `each = \"method-name\"`;")
            }
        }

        // Get the `Group` value
        if let Some(proc_macro2::TokenTree::Group(g)) = attr.tokens.clone().into_iter().next() {
            eprintln!("Group extraction: {:#?}", g);

            let mut ts = g.clone().stream().into_iter();
            eprintln!("Token stream: {:#?}", &ts);

            let each_ident = match ts.next() {
                Some(tt) => if &tt.to_string() != "each" {
                    panic!(MALFORMED_ATTR_MSG);
                } else {
                    tt
                },
                _ => panic!(MALFORMED_ATTR_MSG)
            };

            let eq_punct = match ts.next() {
                Some(tt) =>
                    if &tt.to_string() != "=" {
                        panic!(MALFORMED_ATTR_MSG);
                    } else {
                        tt
                    },
                _ => panic!(MALFORMED_ATTR_MSG)
            };

            if let Some(TokenTree::Literal(fn_name_lit)) = ts.next() {
                // eprintln!("Expected literal: {:#?}", fn_name_lit);
                let fn_name = match syn::Lit::new(fn_name_lit) {
                    syn::Lit::Str(ls) => ls.value(),
                    _ => panic!(MALFORMED_ATTR_MSG)
                };
                let ident = Ident::new(&fn_name, Span::call_site());
                let inner_ty = unwrap_ty_on_tp(always_true, ty);
                match &name {
                    Some(i) => {
                        // if field and requested method name are the same, bail
                        if ident.eq(i) {
                            continue;
                        }
                    }
                    _ => unreachable!()
                }
                method_quotes.push(quote! {
                    pub fn #ident (&mut self, x: #inner_ty) -> &mut Self {
                        match &mut self.#name {
                            Some(xs) => {
                                xs.push(x);
                            },
                            _ => {
                                self.#name = Some(vec![x]);
                            }
                        }
                        self
                    }
                });
            } else {
                panic!(MALFORMED_ATTR_MSG);
            }
        } else {
            panic!(MALFORMED_ATTR_MSG);
        }
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);
    let (_bfields, _bmethods, _bextracts, _bdefaults) = match &_ast.data {
        syn::Data::Enum(_) => panic!("`Builder` can only be derived for `structs`."),
        syn::Data::Union(_) => panic!("`Builder` can only be derived for `structs`."),
        syn::Data::Struct(element) => {
            let mut _fields: Vec<TokenStream> = vec![];
            let mut _methods: Vec<TokenStream> = vec![];
            let mut _extracts: Vec<TokenStream> = vec![];
            let mut _defaults: Vec<TokenStream> = vec![];
            for field in element.fields.iter() {
                let name = &field.ident;
                let ty = &field.ty;
                let resolved_ty = unwrap_ty_on_tp(is_option_type_path, ty);
                let mut is_option_tp: bool = is_type_path_match(is_option_type_path, ty);
                _fields.push(if is_option_tp {
                    quote! {
                        #name: #ty
                    }
                } else {
                    quote! {
                        #name: std::option::Option<#ty>
                    }
                });
                resolve_attrs_methods(field, &mut _methods);
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
        #[derive(Default)]
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
