use crate::attributes::{parse_field_attributes, parse_variant_attributes};
use crate::attributes::{ContainerAttributes, VariantAttributes};
use crate::utils::{get_field_name, get_variant_name, insert_trait_bounds};

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

    let field_names: Vec<_> = fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .collect();
    let field_types: Vec<_> = fields.named.iter().map(|f| &f.ty).collect();
    let field_attributes = fields
        .named
        .iter()
        .map(|field| parse_field_attributes(&field.attrs))
        .collect::<syn::Result<Vec<_>>>()?;

    if field_names.is_empty() {
        return Ok(quote::quote! {
            #(#variant_names)|* => {
                match variant_value {
                    ::celkit::core::Value::Struct(fields) if fields.is_empty() => {
                        Ok(#name::#variant_name {})
                    }
                    _ => Err(::celkit::core::Error::new(format!(
                        "Expected `empty` struct for enum variant `{}`",
                        #variant_name_str,
                    )))
                }
            }
        });
    }

    let expected_fields_array = {
        let field_names_str = field_names
            .iter()
            .zip(field_attributes.iter())
            .filter(|(_, field_attributes)| {
                !field_attributes.skip && !field_attributes.skip_deserializing
            })
            .map(|(field_name, field_attributes)| {
                get_field_name(
                    &field_name.to_string(),
                    &field_attributes,
                    &variant_attributes.container,
                )
            });

        quote::quote! {
            let expected_fields = [#(#field_names_str),*];
        }
    };
    let field_declarations =
        field_names
            .iter()
            .zip(field_types.iter())
            .map(|(field_name, field_type)| {
                quote::quote! {
                    let mut #field_name: Option<#field_type> = None;
                }
            });
    let field_matching = field_names
        .iter()
        .zip(field_types.iter())
        .zip(field_attributes.iter())
        .filter(|((_, _), field_attributes)| {
            !field_attributes.skip && !field_attributes.skip_deserializing
        })
        .map(|((field_name, field_type), field_attributes)| {
            let field_name_str = get_field_name(
                &field_name.to_string(),
                &field_attributes,
                &variant_attributes.container,
            );

            let mut conditions = Vec::from([quote::quote! { field_name == #field_name_str }]);

            for alias in &field_attributes.alias {
                conditions.push(quote::quote! { field_name == #alias });
            }

            quote::quote! {
                if #(#conditions)||* {
                    #field_name = Some(<#field_type>::deserialize(field_value)?);

                    continue;
                }
            }
        });
    let field_assignments = field_names
        .iter()
        .zip(field_types.iter())
        .zip(field_attributes.iter())
        .map(|((field_name, field_type), field_attributes)| {
            if field_attributes.skip || field_attributes.skip_deserializing {
                return quote::quote! {
                    let #field_name = <#field_type>::default();
                };
            }

            if field_attributes.default {
                return quote::quote! {
                    let #field_name = #field_name.unwrap_or_else(|| <#field_type>::default());
                };
            }

            let field_name_str = get_field_name(
                &field_name.to_string(),
                &field_attributes,
                &variant_attributes.container,
            );

            quote::quote! {
                let #field_name = #field_name.ok_or_else(|| {
                    ::celkit::core::Error::new(format!(
                        "Missing field `{}` in variant `{}`",
                        #field_name_str,
                        #variant_name_str,
                    ))
                })?;
            }
        });
    let positional_field_assignments = field_names
        .iter()
        .zip(field_types.iter())
        .zip(field_attributes.iter())
        .map(|((field_name, field_type), field_attributes)| {
            if field_attributes.skip || field_attributes.skip_deserializing {
                return quote::quote! {
                    let #field_name = <#field_type>::default();
                };
            }

            let field_name_str = get_field_name(
                &field_name.to_string(),
                &field_attributes,
                &variant_attributes.container,
            );

            quote::quote! {
                let #field_name = {
                    let (_, field_value) = fields_iter
                        .next()
                        .ok_or_else(|| ::celkit::core::Error::new(format!(
                            "Missing field `{}` in positional deserialization of variant `{}`",
                            #field_name_str,
                            #variant_name_str,
                        )))?;

                    <#field_type>::deserialize(field_value)?
                };
            }
        });
    let variant_construction = quote::quote! {
        Ok(#name::#variant_name {
            #(#field_names),*
        })
    };

    Ok(quote::quote! {
        #(#variant_names)|* => {
            match variant_value {
                ::celkit::core::Value::Struct(fields) => {
                    #expected_fields_array
                    let mut positional = false;

                    if fields.len() == expected_fields.len() {
                        positional = true;

                        for (i, (field_name, _)) in fields.iter().enumerate() {
                            if field_name.is_empty() {
                                // Order must be correct or error
                                break;
                            }

                            if expected_fields[i] != field_name {
                                // Order isn't correct
                                positional = false;

                                break;
                            }
                        }
                    }

                    // Ordered, we match the expected fields
                    if positional {
                        let mut fields_iter = fields.into_iter();

                        #(#positional_field_assignments)*

                        return #variant_construction;
                    }

                    #(#field_declarations)*

                    // Unordered, we search for the values by names
                    for (field_name, field_value) in fields {
                        #(#field_matching)*
                    }

                    // Check whether all of the fields were found
                    #(#field_assignments)*

                    #variant_construction
                }
                _ => Err(::celkit::core::Error::new(format!(
                    "Expected `struct` for enum variant `{}`",
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

    let field_attributes = fields
        .unnamed
        .iter()
        .map(|field| parse_field_attributes(&field.attrs))
        .collect::<syn::Result<Vec<_>>>()?;
    let mut errors: Option<syn::Error> = None;
    let mut has_default_field = false;

    for (i, field_attributes) in field_attributes.iter().enumerate() {
        if has_default_field
            && !field_attributes.default
            && !field_attributes.skip
            && !field_attributes.skip_deserializing
        {
            let error = syn::Error::new_spanned(
                &fields.unnamed[i],
                "Field must have `default/skip` attribute because \
                previous field has `default` attribute",
            );

            match errors {
                Some(ref mut errors) => errors.combine(error),
                None => errors = Some(error),
            }
        }

        if field_attributes.default {
            has_default_field = true;
        }
    }

    if let Some(errors) = errors {
        return Err(errors);
    }

    let field_count = field_attributes
        .iter()
        .filter(|attributes| !attributes.skip && !attributes.skip_deserializing)
        .count();
    let field_types: Vec<_> = fields.unnamed.iter().map(|f| &f.ty).collect();
    let fields = field_types
        .iter()
        .zip(field_attributes.iter())
        .enumerate()
        .map(|(i, (field_type, field_attributes))| {
            let field_ident =
                syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site());

            if field_attributes.skip || field_attributes.skip_deserializing {
                return quote::quote! {
                    let #field_ident = <#field_type>::default();
                };
            }

            if field_attributes.default {
                return quote::quote! {
                    let #field_ident = match fields_iter.next() {
                        Some(field_value) => <#field_type>::deserialize(field_value)?,
                        None => <#field_type>::default(),
                    };
                };
            }

            quote::quote! {
                let #field_ident = <#field_type>::deserialize(
                    fields_iter.next()
                        .ok_or_else(|| ::celkit::core::Error::new(format!(
                            "Missing field {} in variant `{}`",
                            #i,
                            #variant_name_str,
                        )))?
                )?;
            }
        });
    let field_idents = (0..field_types.len())
        .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()));

    Ok(quote::quote! {
        #(#variant_names)|* => {
            match variant_value {
                ::celkit::core::Value::Tuple(fields) => {
                    if fields.len() > #field_count {
                        return Err(::celkit::core::Error::new(format!(
                            "Too many fields for variant `{}`, expected {}, got {}",
                            stringify!(#name),
                            #field_count,
                            fields.len()
                        )));
                    }

                    let mut fields_iter = fields.into_iter();

                    #(#fields)*

                    Ok(#name::#variant_name(#(#field_idents),*))
                }
                _ => Err(::celkit::core::Error::new(format!(
                    "Expected `tuple` for enum variant `{}`",
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
                    "Expected `null` for enum variant `{}`",
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
                                "Unknown `enum` variant `{}` for `{}`",
                                variant_name,
                                stringify!(#name),
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
