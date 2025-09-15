mod attributes;
mod common;
mod deserialize;
mod serialize;
mod utils;

use deserialize::generate_impl_deserialize;
use serialize::generate_impl_serialize;

#[proc_macro_derive(Serialize, attributes(celkit))]
pub fn derive_serialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    generate_impl_serialize(input)
}

#[proc_macro_derive(Deserialize, attributes(celkit))]
pub fn derive_deserialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    generate_impl_deserialize(input)
}
