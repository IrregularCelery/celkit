mod enums;
mod fields;
mod structs;

use enums::generate_enum_serialize;
use structs::generate_struct_serialize;

use crate::attributes::parse_container_attributes;

pub fn generate_impl_serialize(input: syn::DeriveInput) -> proc_macro::TokenStream {
    let name = &input.ident;
    let generics = input.generics;
    let attributes = &input.attrs;

    let container_attributes = match parse_container_attributes(attributes) {
        Ok(attributes) => attributes,
        Err(e) => return e.to_compile_error().into(),
    };

    let impl_serialize = match &input.data {
        syn::Data::Struct(data) => {
            generate_struct_serialize(name, generics, data, &container_attributes)
        }
        syn::Data::Enum(data) => {
            generate_enum_serialize(name, generics, data, &container_attributes)
        }
        syn::Data::Union(_) => Err(syn::Error::new_spanned(
            name,
            "Serialization isn't available for unions",
        )),
    };

    let tokens = match impl_serialize {
        Ok(tokens) => tokens,
        Err(e) => return e.to_compile_error().into(),
    };

    proc_macro::TokenStream::from(tokens)
}
