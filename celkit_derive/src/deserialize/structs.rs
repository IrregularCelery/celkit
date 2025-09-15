use crate::attributes::parse_field_attributes;
use crate::attributes::ContainerAttributes;
use crate::utils::{get_field_name, insert_trait_bounds};

fn generate_named_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsNamed,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
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
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    if field_names.is_empty() {
        return Ok(quote::quote! {
            impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
                fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                    match value {
                        ::celkit::core::Value::Struct(fields) if fields.is_empty() => Ok(#name {}),
                        _ => Err(::celkit::core::Error::new(format!(
                            "Expected `empty` struct for `{}`",
                            stringify!(#name)
                        )))
                    }
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
                    container_attributes,
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
                container_attributes,
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
                container_attributes,
            );

            quote::quote! {
                let #field_name = #field_name.ok_or_else(|| {
                    ::celkit::core::Error::new(format!(
                        "Missing `{}` field in `{}`",
                        #field_name_str,
                        stringify!(#name),
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
                container_attributes,
            );

            quote::quote! {
                let #field_name = {
                    let (_, field_value) = fields_iter
                        .next()
                        .ok_or_else(|| ::celkit::core::Error::new(format!(
                            "Missing field `{}` in positional deserialization of struct `{}`",
                            #field_name_str,
                            stringify!(#name),
                        )))?;

                    <#field_type>::deserialize(field_value)?
                };
            }
        });
    let struct_construction = quote::quote! {
        Ok(#name {
            #(#field_names),*
        })
    };

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
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

                            return #struct_construction;
                        }

                        #(#field_declarations)*

                        // Unordered, we search for the values by names
                        for (field_name, field_value) in fields {
                            #(#field_matching)*
                        }

                        // Check whether all of the fields were found
                        #(#field_assignments)*

                        #struct_construction
                    }
                    _ => Err(::celkit::core::Error::new(format!(
                        "Expected `{}` struct",
                        stringify!(#name)
                    )))
                }
            }
        }
    })
}

fn generate_unnamed_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsUnnamed,
) -> syn::Result<proc_macro2::TokenStream> {
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
                        Some((_, field_value)) => <#field_type>::deserialize(field_value)?,
                        None => <#field_type>::default(),
                    };
                };
            }

            quote::quote! {
                let #field_ident = <#field_type>::deserialize(
                    fields_iter.next()
                        .ok_or_else(|| ::celkit::core::Error::new(format!(
                            "Missing field {} in struct `{}`",
                            #i,
                            stringify!(#name),
                        )))?.1
                )?;
            }
        });
    let field_idents = (0..field_types.len())
        .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()));
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
                    ::celkit::core::Value::Struct(fields) => {
                        if fields.len() > #field_count {
                            return Err(::celkit::core::Error::new(format!(
                                "Too many fields for struct `{}`, expected {}, got {}",
                                stringify!(#name),
                                #field_count,
                                fields.len()
                            )));
                        }

                        let mut fields_iter = fields.into_iter();

                        #(#fields)*

                        Ok(#name(#(#field_idents),*))
                    }
                    _ => Err(::celkit::core::Error::new(format!(
                        "Expected `{}` struct",
                        stringify!(#name),
                    )))
                }
            }
        }
    })
}

fn generate_unit_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
) -> syn::Result<proc_macro2::TokenStream> {
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
                    ::celkit::core::Value::Struct(fields) if fields.is_empty() => Ok(#name),
                    _ => Err(::celkit::core::Error::new(format!(
                        "Expected `empty` struct for `{}`",
                        stringify!(#name),
                    )))
                }
            }
        }
    })
}

pub(super) fn generate_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataStruct,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let fields = &data.fields;

    match fields {
        syn::Fields::Named(fields) => {
            generate_named_struct_deserialize(name, generics, fields, container_attributes)
        }
        syn::Fields::Unnamed(fields) => generate_unnamed_struct_deserialize(name, generics, fields),
        syn::Fields::Unit => generate_unit_struct_deserialize(name, generics),
    }
}
