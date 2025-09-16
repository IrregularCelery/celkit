mod enums;
mod fields;
mod structs;

use enums::generate_enum_deserialize;
use structs::generate_struct_deserialize;

use crate::attributes::parse_container_attributes;

pub fn generate_impl_deserialize(input: syn::DeriveInput) -> proc_macro::TokenStream {
    let name = &input.ident;
    let generics = input.generics;
    let attributes = &input.attrs;

    let container_attributes = match parse_container_attributes(attributes) {
        Ok(attributes) => attributes,
        Err(e) => return e.to_compile_error().into(),
    };

    let impl_deserialize = match &input.data {
        syn::Data::Struct(data) => {
            generate_struct_deserialize(name, generics, data, &container_attributes)
        }
        syn::Data::Enum(data) => {
            generate_enum_deserialize(name, generics, data, &container_attributes)
        }
        syn::Data::Union(_) => {
            return syn::Error::new_spanned(name, "Serialization isn't available for unions")
                .to_compile_error()
                .into();
        }
    };

    let tokens = match impl_deserialize {
        Ok(tokens) => tokens,
        Err(e) => return e.to_compile_error().into(),
    };

    proc_macro::TokenStream::from(tokens)
}
