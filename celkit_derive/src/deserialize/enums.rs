use crate::attributes::parse_variant_attributes;
use crate::attributes::{ContainerAttributes, VariantAttributes};
use crate::utils::{get_variant_name, insert_trait_bounds};

use super::fields::NamedFieldHandler;
use super::fields::UnnamedFieldHandler;

fn generate_variant_pattern(
    variant_name_str: String,
    variant_index: usize,
    variant_attributes: &VariantAttributes,
    by_index: bool,
) -> proc_macro2::TokenStream {
    if by_index {
        let index_lit =
            syn::LitInt::new(&variant_index.to_string(), proc_macro2::Span::call_site());

        return quote::quote! { #index_lit };
    }

    let mut variant_names = Vec::from([quote::quote! { #variant_name_str }]);

    for alias in &variant_attributes.alias {
        variant_names.push(quote::quote! { #alias });
    }

    quote::quote! { #(#variant_names)|* }
}

fn generate_named_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    variant_index: usize,
    fields: &syn::FieldsNamed,
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
    by_index: bool,
) -> syn::Result<proc_macro2::TokenStream> {
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );
    let pattern = generate_variant_pattern(
        variant_name_str.clone(),
        variant_index,
        variant_attributes,
        by_index,
    );

    if variant_attributes.skip || variant_attributes.skip_deserializing {
        return Ok(quote::quote! {
            #pattern => {
                return Err(::celkit::core::Error::new(format!(
                    "Enum variant `{}::{}` cannot be deserialized",
                    stringify!(#name),
                    stringify!(#variant_name),
                )));
            }
        });
    }

    let field_handler = NamedFieldHandler::new(fields, &variant_attributes.container)?;
    let field_names = field_handler.field_names();

    if field_names.is_empty() {
        return Ok(quote::quote! {
            #pattern => {
                match payload {
                    ::celkit::core::Value::Struct(fields) if fields.is_empty() => {
                        Ok(#name::#variant_name {})
                    }
                    _ => Err(::celkit::core::Error::new(format!(
                        "Expected `empty` struct for enum variant `{}::{}`",
                        stringify!(#name),
                        #variant_name_str,
                    )))
                }
            }
        });
    }

    let construction = quote::quote! {
        Ok(#name::#variant_name {
            #(#field_names),*
        })
    };
    let context_name = format!("variant `{}::{}`", name.to_string(), variant_name_str);
    let field_deserialization =
        field_handler.generate_fields_deserialize(construction, &context_name);

    Ok(quote::quote! {
        #pattern => {
            match payload {
                ::celkit::core::Value::Struct(fields) => {
                    #field_deserialization
                }
                _ => Err(::celkit::core::Error::new(format!(
                    "Expected `struct` for enum variant `{}::{}`",
                    stringify!(#name),
                    #variant_name_str,
                )))
            }
        }
    })
}

fn generate_unnamed_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    variant_index: usize,
    fields: &syn::FieldsUnnamed,
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
    by_index: bool,
) -> syn::Result<proc_macro2::TokenStream> {
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );
    let pattern = generate_variant_pattern(
        variant_name_str.clone(),
        variant_index,
        variant_attributes,
        by_index,
    );

    if variant_attributes.skip || variant_attributes.skip_deserializing {
        return Ok(quote::quote! {
            #pattern => {
                return Err(::celkit::core::Error::new(format!(
                    "Enum variant `{}::{}` cannot be deserialized",
                    stringify!(#name),
                    stringify!(#variant_name),
                )));
            }
        });
    }

    let field_handler = UnnamedFieldHandler::new(fields)?;
    let field_names = field_handler.field_names();
    let construction = quote::quote! { Ok(#name::#variant_name(#(#field_names),*)) };
    let context_name = format!("variant `{}::{}`", name.to_string(), variant_name_str);
    let fields_format = quote::quote! { field_value };
    let field_deserialization =
        field_handler.generate_fields_deserialize(construction, &context_name, fields_format);

    Ok(quote::quote! {
        #pattern => {
            match payload {
                ::celkit::core::Value::Tuple(fields) => {
                    #field_deserialization
                }
                _ => Err(::celkit::core::Error::new(format!(
                    "Expected `tuple` for enum variant `{}::{}`",
                    stringify!(#name),
                    #variant_name_str,
                )))
            }
        }
    })
}

fn generate_unit_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    variant_index: usize,
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
    by_index: bool,
) -> syn::Result<proc_macro2::TokenStream> {
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );
    let pattern = generate_variant_pattern(
        variant_name_str.clone(),
        variant_index,
        variant_attributes,
        by_index,
    );

    if variant_attributes.skip || variant_attributes.skip_deserializing {
        return Ok(quote::quote! {
            #pattern => {
                return Err(::celkit::core::Error::new(format!(
                    "Enum variant `{}::{}` cannot be deserialized",
                    stringify!(#name),
                    stringify!(#variant_name),
                )));
            }
        });
    }

    Ok(quote::quote! {
        #pattern => {
            match payload {
                ::celkit::core::Value::Null => Ok(#name::#variant_name),
                _ => Err(::celkit::core::Error::new(format!(
                    "Expected `null` for enum variant `{}::{}`",
                    stringify!(#name),
                    #variant_name_str,
                )))
            }
        }
    })
}

pub(super) fn generate_enum_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataEnum,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    if data.variants.is_empty() {
        return Ok(quote::quote! {
            impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
                fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                    Err(::celkit::core::Error::new(format!(
                        "Cannot `deserialize` empty enum `{}`",
                        stringify!(#name)
                    )))
                }
            }
        });
    }

    let variants_by_index = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let variant_name = &variant.ident;
            let variant_index = i;
            let attributes = &variant.attrs;
            let variant_attributes = parse_variant_attributes(attributes)?;

            match &variant.fields {
                syn::Fields::Named(fields) => generate_named_enum_deserialize(
                    name,
                    variant_name,
                    variant_index,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                    true,
                ),
                syn::Fields::Unnamed(fields) => generate_unnamed_enum_deserialize(
                    name,
                    variant_name,
                    variant_index,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                    true,
                ),
                syn::Fields::Unit => generate_unit_enum_deserialize(
                    name,
                    variant_name,
                    variant_index,
                    &container_attributes,
                    &variant_attributes,
                    true,
                ),
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let variants_by_name = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let variant_name = &variant.ident;
            let variant_index = i;
            let attributes = &variant.attrs;
            let variant_attributes = parse_variant_attributes(attributes)?;

            match &variant.fields {
                syn::Fields::Named(fields) => generate_named_enum_deserialize(
                    name,
                    variant_name,
                    variant_index,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                    false,
                ),
                syn::Fields::Unnamed(fields) => generate_unnamed_enum_deserialize(
                    name,
                    variant_name,
                    variant_index,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                    false,
                ),
                syn::Fields::Unit => generate_unit_enum_deserialize(
                    name,
                    variant_name,
                    variant_index,
                    &container_attributes,
                    &variant_attributes,
                    false,
                ),
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
                    ::celkit::core::Value::Tuple(mut tuple) => {
                        if tuple.len() != 2 {
                            return Err(::celkit::core::Error::new(format!(
                                "Expected `enum` tuple with exactly two members \
                                (variant_index, payload) for `{}`",
                                stringify!(#name),
                            )));
                        }

                        // Pop in reverse order
                        let payload = tuple
                            .pop()
                            .expect("This SHOULD never happen because of length check!");
                        let discriminant = tuple
                            .pop()
                            .expect("This SHOULD never happen because of length check!");

                        let variant_index = match discriminant {
                            ::celkit::core::Value::Number(number) => number.as_usize()?,
                            _ => {
                                return Err(::celkit::core::Error::new(
                                    "First member of `Duration` tuple must be a number \
                                    for `variant_index`",
                                ));
                            }
                        };

                        match variant_index {
                            #(#variants_by_index)*
                            _ => Err(::celkit::core::Error::new(format!(
                                "Unknown `enum` variant index {} for `{}`",
                                variant_index,
                                stringify!(#name),
                            )))
                        }
                    }
                    ::celkit::core::Value::Object(mut object) => {
                        if object.len() != 1 {
                            return Err(::celkit::core::Error::new(format!(
                                "Expected `enum` object with exactly one entry for `{}`",
                                stringify!(#name),
                            )));
                        }

                        let (variant_name, payload) = object.into_iter().next()
                            .ok_or_else(|| ::celkit::core::Error::new(format!(
                                "Empty `enum` object while deserializing `{}`",
                                stringify!(#name),
                            )))?;

                        match variant_name.as_str() {
                            #(#variants_by_name)*
                            _ => Err(::celkit::core::Error::new(format!(
                                "Unknown `enum` variant `{}::{}`",
                                stringify!(#name),
                                variant_name,
                            )))
                        }
                    }
                    _ => Err(::celkit::core::Error::new(format!(
                        "Expected `enum` object for `{}`",
                        stringify!(#name)
                    )))
                }
            }
        }
    })
}
