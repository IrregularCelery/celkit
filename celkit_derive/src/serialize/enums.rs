// NOTE:
//
// Special-type serialization (copied from `celkit-core::impls.rs`)
//
// Some special types (e.g. Result, Duration) are represented as structs with reserved field names
// that tell encoders how to handle them.
//
// Reserved field names:
// - "0": A marker indicating the struct is "special" and must be handled differently by encoders.
//   (This field is always present in special structs)
// - "#": Type-specific discriminants or values that are ignored by verbose encoders (e.g. string),
//   but kept by data-only encoders (e.g. binary).
//   If only one such field exists, it can be flattened instead of wrapped.
// - "%": Values that are the opposite of "#"; they are ignored by data-only encoders (e.g. binary),
//   but kept by verbose encoders (e.g. string).
//   If only one such field exists, it can be flattened instead of wrapped.
// - Normal names ("Ok", "secs", ...): Used by verbose encoders. For data-only encoders, the names
//   are ignored, but the values are kept in positional order.
//
// This design ensures any encoder can have access to necessary values and data while having the
// ability to ignore data that is unnecessary to them.
//
// The "0", "#", and "%" names were chosen to guarantee no conflicts with user-defined field names.

use crate::attributes::parse_variant_attributes;
use crate::attributes::{ContainerAttributes, VariantAttributes};
use crate::utils::{get_variant_name, insert_trait_bounds};

use super::fields::{NamedFieldHandler, UnnamedFieldHandler};

fn generate_named_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    variant_index: usize,
    fields: &syn::FieldsNamed,
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    if variant_attributes.skip || variant_attributes.skip_serializing {
        return Ok(quote::quote! {
            #name::#variant_name { .. } => {
                return Err(::celkit::core::Error::new(format!(
                    "Enum variant `{}::{}` cannot be serialized",
                    stringify!(#name),
                    stringify!(#variant_name),
                )));
            }
        });
    }

    let field_handler = NamedFieldHandler::new(fields)?;
    let field_names = field_handler.field_names();
    let field_serialization =
        field_handler.generate_fields_serialize(&variant_attributes.container, quote::quote! {});
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );

    Ok(quote::quote! {
        #name::#variant_name { #(#field_names),* } => {
            #field_serialization

            let mut values = Vec::with_capacity(3);
            let variant_value = ::celkit::core::Value::Struct(fields);

            // See: "Special-type serialization" at the top of this file
            values.push(("0".to_string(), Value::Null));
            values.push(("#".to_string(), Value::Number(Number::Usize(#variant_index))));
            values.push((#variant_name_str.to_string(), variant_value));

            Ok(::celkit::core::Value::Struct(values))
        }
    })
}

fn generate_unnamed_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    variant_index: usize,
    fields: &syn::FieldsUnnamed,
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    if variant_attributes.skip || variant_attributes.skip_serializing {
        return Ok(quote::quote! {
            #name::#variant_name { .. } => {
                return Err(::celkit::core::Error::new(format!(
                    "Enum variant `{}::{}` cannot be serialized",
                    stringify!(#name),
                    stringify!(#variant_name),
                )));
            }
        });
    }

    let field_handler = UnnamedFieldHandler::new(fields)?;
    let field_names = field_handler.field_names();
    let field_serialization = field_handler.generate_fields_serialize(false);
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );

    Ok(quote::quote! {
        #name::#variant_name(#(#field_names),*) => {
            #field_serialization

            let mut values = Vec::with_capacity(3);
            let variant_value = ::celkit::core::Value::Tuple(fields);

            // See: "Special-type serialization" at the top of this file
            values.push(("0".to_string(), Value::Null));
            values.push(("#".to_string(), Value::Number(Number::Usize(#variant_index))));
            values.push((#variant_name_str.to_string(), variant_value));

            Ok(::celkit::core::Value::Struct(values))
        }
    })
}

pub(super) fn generate_unit_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    variant_index: usize,
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    if variant_attributes.skip || variant_attributes.skip_serializing {
        return Ok(quote::quote! {
            #name::#variant_name { .. } => {
                return Err(::celkit::core::Error::new(format!(
                    "Enum variant `{}::{}` cannot be serialized",
                    stringify!(#name),
                    stringify!(#variant_name),
                )));
            }
        });
    }

    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );

    Ok(quote::quote! {
        #name::#variant_name => {
            use ::celkit::core::*;

            let mut values = Vec::with_capacity(3);
            let variant_value = ::celkit::core::Value::Null;

            // See: "Special-type serialization" at the top of this file
            values.push(("0".to_string(), Value::Null));
            values.push(("#".to_string(), Value::Number(Number::Usize(#variant_index))));
            values.push((#variant_name_str.to_string(), variant_value));

            Ok(::celkit::core::Value::Struct(values))
        }
    })
}

pub(super) fn generate_enum_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataEnum,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    if data.variants.is_empty() {
        return Ok(quote::quote! {
            impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
                fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                    // This is unreachable because you cannot construct a value of empty enum
                    match *self {}
                }
            }
        });
    }

    let variants = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let variant_name = &variant.ident;
            let variant_index = i;
            let attributes = &variant.attrs;
            let variant_attributes = parse_variant_attributes(attributes)?;

            match &variant.fields {
                syn::Fields::Named(fields) => generate_named_enum_serialize(
                    name,
                    variant_name,
                    variant_index,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                ),
                syn::Fields::Unnamed(fields) => generate_unnamed_enum_serialize(
                    name,
                    variant_name,
                    variant_index,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                ),
                syn::Fields::Unit => generate_unit_enum_serialize(
                    name,
                    variant_name,
                    variant_index,
                    &container_attributes,
                    &variant_attributes,
                ),
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                match self {
                    #(#variants)*
                }
            }
        }
    })
}
