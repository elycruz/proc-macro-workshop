extern crate proc_macro;
extern crate syn;

use quote::quote;

struct BuilderAttribute {

}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);

    let _expanded = quote! {

    };

    return _expanded;
}