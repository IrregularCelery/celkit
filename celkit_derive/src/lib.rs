#[proc_macro_derive(Serialize)]
pub fn derive_serialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &input.ident;

    let impl_serialize = match &input.data {
        syn::Data::Struct(data_struct) => generate_struct_serialize(name, &data_struct.fields),
        syn::Data::Enum(data_enum) => todo!(),
        syn::Data::Union(_data_union) => {
            return syn::Error::new_spanned(name, "Serialization isn't available for unions")
                .to_compile_error()
                .into();
        }
    };

    proc_macro::TokenStream::from(impl_serialize)
}

#[proc_macro_derive(Deserialize)]
pub fn derive_deserialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &input.ident;

    let impl_deserialize = match &input.data {
        syn::Data::Struct(data_struct) => generate_struct_deserialize(name, &data_struct.fields),
        syn::Data::Enum(data_enum) => todo!(),
        syn::Data::Union(_data_union) => {
            return syn::Error::new_spanned(name, "Serialization isn't available for unions")
                .to_compile_error()
                .into();
        }
    };

    proc_macro::TokenStream::from(impl_deserialize)
}

// ------------------------------ Serialize ------------------------------- //

fn generate_named_fields_struct_serialize(
    name: &syn::Ident,
    fields: &syn::FieldsNamed,
) -> proc_macro2::TokenStream {
    let field_count = fields.named.len();
    let fields = fields.named.iter().map(|field| {
        let field_name = match &field.ident {
            Some(ident) => ident,
            None => {
                return syn::Error::new_spanned(field, "Named field must have an identifier")
                    .to_compile_error();
            }
        };

        quote::quote! {
            fields.push((
                ::celkit::core::utils::unescape_identifier(stringify!(#field_name)).to_string(),
                self.#field_name.serialize()?
            ));
        }
    });

    quote::quote! {
        impl ::celkit::core::Serialize for #name {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                let mut fields = Vec::with_capacity(#field_count);

                #(#fields)*

                Ok(::celkit::core::Value::Struct(fields))
            }
        }
    }
}

fn generate_unnamed_fields_struct_serialize(
    name: &syn::Ident,
    fields: &syn::FieldsUnnamed,
) -> proc_macro2::TokenStream {
    let field_count = fields.unnamed.len();
    let fields = fields.unnamed.iter().enumerate().map(|(i, _)| {
        let index = syn::Index::from(i);

        quote::quote! {
            fields.push(self.#index.serialize()?);
        }
    });

    quote::quote! {
        impl ::celkit::core::Serialize for #name {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                let mut fields = Vec::with_capacity(#field_count);

                #(#fields)*

                Ok(::celkit::core::Value::Tuple(fields)) // TODO: Look into this, might wanna have
                                                         // different format
            }
        }
    }
}

fn generate_unit_fields_struct_serialize(name: &syn::Ident) -> proc_macro2::TokenStream {
    quote::quote! {
        impl ::celkit::core::Serialize for #name {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                Ok(::celkit::core::Value::Struct(Vec::new()))
            }
        }
    }
}

fn generate_struct_serialize(name: &syn::Ident, fields: &syn::Fields) -> proc_macro2::TokenStream {
    match fields {
        syn::Fields::Named(fields_named) => {
            generate_named_fields_struct_serialize(name, fields_named)
        }
        syn::Fields::Unnamed(fields_unnamed) => {
            generate_unnamed_fields_struct_serialize(name, fields_unnamed)
        }
        syn::Fields::Unit => generate_unit_fields_struct_serialize(name),
    }
}

// ----------------------------- Deserialize ------------------------------ //

fn generate_named_fields_struct_deserialize(
    name: &syn::Ident,
    fields: &syn::FieldsNamed,
) -> proc_macro2::TokenStream {
    let field_names: Vec<_> = fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .collect();
    let field_types: Vec<_> = fields.named.iter().map(|f| &f.ty).collect();

    if field_names.is_empty() {
        return quote::quote! {
            impl ::celkit::core::Deserialize for #name {
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
        };
    }

    let field_idents: Vec<_> = field_names.iter().map(|name| name.to_string()).collect();
    let expected_fields_array = quote::quote! {
        let expected_fields = [
            #(::celkit::core::utils::unescape_identifier(stringify!(#field_idents))),*
        ];
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
    let field_matching =
        field_names
            .iter()
            .zip(field_types.iter())
            .map(|(field_name, field_type)| {
                quote::quote! {
                    if field_name
                        == ::celkit::core::utils::unescape_identifier(stringify!(#field_name))
                    {
                        #field_name = Some(<#field_type>::deserialize(field_value)?);

                        continue;
                    }
                }
            });
    let field_assignments = field_names.iter().map(|field_name| {
        quote::quote! {
            let #field_name = #field_name.ok_or_else(|| {
                ::celkit::core::Error::new(format!(
                    "Missing `{}` field in `{}`",
                    ::celkit::core::utils::unescape_identifier(stringify!(#field_name)),
                    stringify!(#name),
                ))
            })?;
        }
    });
    let positional_field_assignments =
        field_names
            .iter()
            .zip(field_types.iter())
            .map(|(field_name, field_type)| {
                quote::quote! {
                    let #field_name = {
                        let (_, field_value) = fields_iter
                            .next()
                            .ok_or_else(|| ::celkit::core::Error::new(format!(
                                "Missing field `{}` in positional deserialization of struct `{}`",
                                ::celkit::core::utils::unescape_identifier(stringify!(#field_name)),
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

    quote::quote! {
        impl ::celkit::core::Deserialize for #name {
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

                                if i >= expected_fields.len() || expected_fields[i] != field_name {
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
    }
}

fn generate_unnamed_fields_struct_deserialize(
    name: &syn::Ident,
    fields: &syn::FieldsUnnamed,
) -> proc_macro2::TokenStream {
    let field_count = fields.unnamed.len();
    let field_types: Vec<_> = fields.unnamed.iter().map(|f| &f.ty).collect();
    let fields = field_types.iter().enumerate().map(|(i, field_type)| {
        let field_ident = syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site());

        quote::quote! {
            let #field_ident = <#field_type>::deserialize(
                fields_iter.next()
                    .ok_or_else(|| ::celkit::core::Error::new(format!(
                        "Missing field {} in struct `{}`",
                        #i,
                        stringify!(#name),
                    )))?
            )?;
        }
    });

    let field_idents = (0..field_count)
        .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()));

    quote::quote! {
        impl ::celkit::core::Deserialize for #name {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
                    ::celkit::core::Value::Tuple(fields) => {
                        if fields.len() != #field_count {
                            return Err(::celkit::core::Error::new(format!(
                                "Expected {} fields for struct `{}`, got {}",
                                #field_count,
                                stringify!(#name),
                                fields.len()
                            )));
                        }

                        let mut fields_iter = fields.into_iter();

                        #(#fields)*

                        Ok(#name(#(#field_idents),*))
                    }
                    _ => Err(::celkit::core::Error::new(format!(
                        "Expected `tuple` for struct `{}`",
                        stringify!(#name),
                    )))
                }
            }
        }
    }
}

fn generate_unit_fields_struct_deserialize(name: &syn::Ident) -> proc_macro2::TokenStream {
    quote::quote! {
        impl ::celkit::core::Deserialize for #name {
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
    }
}

fn generate_struct_deserialize(
    name: &syn::Ident,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    match fields {
        syn::Fields::Named(fields_named) => {
            generate_named_fields_struct_deserialize(name, fields_named)
        }
        syn::Fields::Unnamed(fields_unnamed) => {
            generate_unnamed_fields_struct_deserialize(name, fields_unnamed)
        }
        syn::Fields::Unit => generate_unit_fields_struct_deserialize(name),
    }
}
