use crate::attributes::parse_variant_attributes;
use crate::attributes::{ContainerAttributes, VariantAttributes};
use crate::utils::{get_variant_name, insert_trait_bounds};

use super::fields::{generate_named_fields_deserialize, UnnamedFieldHandler};
use super::fields::{generate_unnamed_fields_deserialize, NamedFieldHandler};

fn generate_named_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    fields: &syn::FieldsNamed,
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );
    let mut variant_names = Vec::from([quote::quote! { #variant_name_str }]);

    for alias in &variant_attributes.alias {
        variant_names.push(quote::quote! { #alias });
    }

    if variant_attributes.skip || variant_attributes.skip_deserializing {
        return Ok(quote::quote! {
            #(#variant_names)|* => {
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
            #(#variant_names)|* => {
                match variant_value {
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
    let deserialization =
        generate_named_fields_deserialize(&field_handler, construction, &context_name);

    Ok(quote::quote! {
        #(#variant_names)|* => {
            match variant_value {
                ::celkit::core::Value::Struct(fields) => {
                    #deserialization
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
    fields: &syn::FieldsUnnamed,
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );
    let mut variant_names = Vec::from([quote::quote! { #variant_name_str }]);

    for alias in &variant_attributes.alias {
        variant_names.push(quote::quote! { #alias });
    }

    if variant_attributes.skip || variant_attributes.skip_deserializing {
        return Ok(quote::quote! {
            #(#variant_names)|* => {
                return Err(::celkit::core::Error::new(format!(
                    "Enum variant `{}::{}` cannot be deserialized",
                    stringify!(#name),
                    stringify!(#variant_name),
                )));
            }
        });
    }

    let field_handler = UnnamedFieldHandler::new(fields)?;
    let field_idents = field_handler.field_idents();
    let construction = quote::quote! { Ok(#name::#variant_name(#(#field_idents),*)) };
    let context_name = format!("variant `{}::{}`", name.to_string(), variant_name_str);
    let fields_format = quote::quote! { field_value };
    let deserialization = generate_unnamed_fields_deserialize(
        &field_handler,
        construction,
        &context_name,
        fields_format,
    );

    Ok(quote::quote! {
        #(#variant_names)|* => {
            match variant_value {
                ::celkit::core::Value::Tuple(fields) => {
                    #deserialization
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
    container_attributes: &ContainerAttributes,
    variant_attributes: &VariantAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );
    let mut variant_names = Vec::from([quote::quote! { #variant_name_str }]);

    for alias in &variant_attributes.alias {
        variant_names.push(quote::quote! { #alias });
    }

    if variant_attributes.skip || variant_attributes.skip_deserializing {
        return Ok(quote::quote! {
            #(#variant_names)|* => {
                return Err(::celkit::core::Error::new(format!(
                    "Enum variant `{}::{}` cannot be deserialized",
                    stringify!(#name),
                    stringify!(#variant_name),
                )));
            }
        });
    }

    Ok(quote::quote! {
        #(#variant_names)|* => {
            match variant_value {
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

    let variants = data
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            let attributes = &variant.attrs;
            let variant_attributes = parse_variant_attributes(attributes)?;

            match &variant.fields {
                syn::Fields::Named(fields) => generate_named_enum_deserialize(
                    name,
                    variant_name,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                ),
                syn::Fields::Unnamed(fields) => generate_unnamed_enum_deserialize(
                    name,
                    variant_name,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                ),
                syn::Fields::Unit => generate_unit_enum_deserialize(
                    name,
                    variant_name,
                    &container_attributes,
                    &variant_attributes,
                ),
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
                    ::celkit::core::Value::Object(mut object) => {
                        if object.len() != 1 {
                            return Err(::celkit::core::Error::new(format!(
                                "Expected `enum` object with exactly one entry for `{}`",
                                stringify!(#name),
                            )));
                        }

                        let (variant_name, variant_value) = object.into_iter().next()
                            .ok_or_else(|| ::celkit::core::Error::new(format!(
                                "Empty `enum` object while deserializing `{}`",
                                stringify!(#name),
                            )))?;

                        match variant_name.as_str() {
                            #(#variants)*
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
