// TODO: Add `container` and `field` attributes
#[proc_macro_derive(Serialize)]
pub fn derive_serialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &input.ident;
    let generics = input.generics;

    let impl_serialize = match &input.data {
        syn::Data::Struct(data) => generate_struct_serialize(name, generics, data),
        syn::Data::Enum(data) => generate_enum_serialize(name, generics, data),
        syn::Data::Union(_) => {
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
    let generics = input.generics;

    let impl_deserialize = match &input.data {
        syn::Data::Struct(data) => generate_struct_deserialize(name, generics, data),
        syn::Data::Enum(data) => generate_enum_deserialize(name, generics, data),
        syn::Data::Union(_) => {
            return syn::Error::new_spanned(name, "Serialization isn't available for unions")
                .to_compile_error()
                .into();
        }
    };

    proc_macro::TokenStream::from(impl_deserialize)
}

fn insert_trait_bounds(mut generics: syn::Generics, trait_name: &str) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            let trait_ident: syn::Ident = syn::parse_str(trait_name).expect("Invalid trait name");

            type_param
                .bounds
                .push(syn::parse_quote!(::celkit::#trait_ident));
        }
    }

    generics
}

// ------------------------ Struct Serialization -------------------------- //

fn generate_named_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsNamed,
) -> proc_macro2::TokenStream {
    let field_count = fields.named.len();
    let fields = fields.named.iter().map(|field| {
        let Some(field_name) = &field.ident else {
            return syn::Error::new_spanned(field, "Named field must have an identifier")
                .to_compile_error();
        };

        quote::quote! {
            fields.push((
                ::celkit::core::utils::unescape_identifier(stringify!(#field_name)).to_string(),
                self.#field_name.serialize()?
            ));
        }
    });
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                let mut fields = Vec::with_capacity(#field_count);

                #(#fields)*

                Ok(::celkit::core::Value::Struct(fields))
            }
        }
    }
}

fn generate_unnamed_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsUnnamed,
) -> proc_macro2::TokenStream {
    let field_count = fields.unnamed.len();
    let fields = fields.unnamed.iter().enumerate().map(|(i, _)| {
        let index = syn::Index::from(i);

        quote::quote! {
            fields.push((String::new(), self.#index.serialize()?));
        }
    });
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                let mut fields = Vec::with_capacity(#field_count);

                #(#fields)*

                Ok(::celkit::core::Value::Struct(fields))
            }
        }
    }
}

fn generate_unit_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
) -> proc_macro2::TokenStream {
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                Ok(::celkit::core::Value::Struct(Vec::new()))
            }
        }
    }
}

fn generate_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataStruct,
) -> proc_macro2::TokenStream {
    let fields = &data.fields;

    match fields {
        syn::Fields::Named(fields) => generate_named_struct_serialize(name, generics, fields),
        syn::Fields::Unnamed(fields) => generate_unnamed_struct_serialize(name, generics, fields),
        syn::Fields::Unit => generate_unit_struct_serialize(name, generics),
    }
}

// ----------------------- Struct Deserialization ------------------------- //

fn generate_named_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsNamed,
) -> proc_macro2::TokenStream {
    let field_names: Vec<_> = fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .collect();
    let field_types: Vec<_> = fields.named.iter().map(|f| &f.ty).collect();
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    if field_names.is_empty() {
        return quote::quote! {
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
        };
    }

    let expected_fields_array = quote::quote! {
        let expected_fields = [
            #(::celkit::core::utils::unescape_identifier(stringify!(#field_names))),*
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

fn generate_unnamed_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
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
                    )))?.1
            )?;
        }
    });
    let field_idents = (0..field_count)
        .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()));
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
                    ::celkit::core::Value::Struct(fields) => {
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
                        "Expected `{}` struct",
                        stringify!(#name),
                    )))
                }
            }
        }
    }
}

fn generate_unit_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
) -> proc_macro2::TokenStream {
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
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
    }
}

fn generate_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataStruct,
) -> proc_macro2::TokenStream {
    let fields = &data.fields;

    match fields {
        syn::Fields::Named(fields) => generate_named_struct_deserialize(name, generics, fields),
        syn::Fields::Unnamed(fields) => generate_unnamed_struct_deserialize(name, generics, fields),
        syn::Fields::Unit => generate_unit_struct_deserialize(name, generics),
    }
}

// ------------------------- Enum Serialization --------------------------- //

fn generate_named_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    fields: &syn::FieldsNamed,
) -> proc_macro2::TokenStream {
    let field_count = fields.named.len();
    let field_names: Vec<_> = fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .collect();
    let fields = field_names.iter().map(|field_name| {
        quote::quote! {
            fields.push((
                ::celkit::core::utils::unescape_identifier(stringify!(#field_name)).to_string(),
                #field_name.serialize()?
            ));
        }
    });

    quote::quote! {
        #name::#variant_name { #(#field_names),* } => {
            use ::celkit::core::*;

            let mut fields = Vec::with_capacity(#field_count);

            #(#fields)*

            let variant_value = ::celkit::core::Value::Struct(fields);
            let mut variant_object = BTreeMap::new();

            variant_object.insert(stringify!(#variant_name).to_string(), variant_value);

            Ok(::celkit::core::Value::Object(variant_object))
        }
    }
}

fn generate_unnamed_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    fields: &syn::FieldsUnnamed,
) -> proc_macro2::TokenStream {
    let field_count = fields.unnamed.len();
    let field_names: Vec<_> = (0..field_count)
        .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()))
        .collect();
    let fields = field_names.iter().map(|field_name| {
        quote::quote! {
            fields.push(#field_name.serialize()?);
        }
    });

    quote::quote! {
        #name::#variant_name(#(#field_names),*) => {
            use ::celkit::core::*;

            let mut fields = Vec::with_capacity(#field_count);

            #(#fields)*

            let variant_value = ::celkit::core::Value::Tuple(fields);
            let mut variant_object = BTreeMap::new();

            variant_object.insert(stringify!(#variant_name).to_string(), variant_value);

            Ok(::celkit::core::Value::Object(variant_object))
        }
    }
}

fn generate_unit_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
) -> proc_macro2::TokenStream {
    quote::quote! {
        #name::#variant_name => {
            use ::celkit::core::*;

            let mut variant_object = BTreeMap::new();

            variant_object.insert(stringify!(#variant_name).to_string(), ::celkit::core::Value::Null);

            Ok(::celkit::core::Value::Object(variant_object))
        }
    }
}

fn generate_enum_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataEnum,
) -> proc_macro2::TokenStream {
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    if data.variants.is_empty() {
        return quote::quote! {
            impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
                fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                    // This is unreachable because you cannot construct a value of empty enum
                    match *self {}
                }
            }
        };
    }

    let variants = data.variants.iter().map(|variant| {
        let variant_name = &variant.ident;

        match &variant.fields {
            syn::Fields::Named(fields) => generate_named_enum_serialize(name, variant_name, fields),
            syn::Fields::Unnamed(fields) => {
                generate_unnamed_enum_serialize(name, variant_name, fields)
            }
            syn::Fields::Unit => generate_unit_enum_serialize(name, variant_name),
        }
    });

    quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                match self {
                    #(#variants)*
                }
            }
        }
    }
}

// ------------------------ Enum Deserialization -------------------------- //

fn generate_named_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
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
            stringify!(#variant_name) => {
                match variant_value {
                    ::celkit::core::Value::Struct(fields) if fields.is_empty() => {
                        Ok(#name::#variant_name {})
                    }
                    _ => Err(::celkit::core::Error::new(format!(
                        "Expected `empty` struct for enum variant `{}`",
                        stringify!(#variant_name)
                    )))
                }
            }
        };
    }

    let expected_fields_array = quote::quote! {
        let expected_fields = [
            #(::celkit::core::utils::unescape_identifier(stringify!(#field_names))),*
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
                    "Missing field `{}` in variant `{}`",
                    ::celkit::core::utils::unescape_identifier(stringify!(#field_name)),
                    stringify!(#variant_name),
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
                                "Missing field `{}` in positional deserialization of variant `{}`",
                                ::celkit::core::utils::unescape_identifier(stringify!(#field_name)),
                                stringify!(#variant_name),
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

    quote::quote! {
        stringify!(#variant_name) => {
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
                    stringify!(#variant_name),
                )))
            }
        }
    }
}

fn generate_unnamed_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
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
                        "Missing field {} in variant `{}`",
                        #i,
                        stringify!(#variant_name),
                    )))?
            )?;
        }
    });
    let field_idents = (0..field_count)
        .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()));

    quote::quote! {
        stringify!(#variant_name) => {
            match variant_value {
                ::celkit::core::Value::Tuple(fields) => {
                    let mut fields_iter = fields.into_iter();

                    #(#fields)*

                    Ok(#name::#variant_name(#(#field_idents),*))
                }
                _ => Err(::celkit::core::Error::new(format!(
                    "Expected `tuple` for enum variant `{}`",
                    stringify!(#variant_name),
                )))
            }
        }
    }
}

fn generate_unit_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
) -> proc_macro2::TokenStream {
    quote::quote! {
        stringify!(#variant_name) => {
            match variant_value {
                ::celkit::core::Value::Null => Ok(#name::#variant_name),
                _ => Err(::celkit::core::Error::new(format!(
                    "Expected `null` for enum variant `{}`",
                    stringify!(#variant_name),
                )))
            }
        }
    }
}

fn generate_enum_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataEnum,
) -> proc_macro2::TokenStream {
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    if data.variants.is_empty() {
        return quote::quote! {
            impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
                fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                    Err(::celkit::core::Error::new(format!(
                        "Cannot `deserialize` empty enum `{}`",
                        stringify!(#name)
                    )))
                }
            }
        };
    }

    let variants = data.variants.iter().map(|variant| {
        let variant_name = &variant.ident;

        match &variant.fields {
            syn::Fields::Named(fields) => {
                generate_named_enum_deserialize(name, variant_name, fields)
            }
            syn::Fields::Unnamed(fields) => {
                generate_unnamed_enum_deserialize(name, variant_name, fields)
            }
            syn::Fields::Unit => generate_unit_enum_deserialize(name, variant_name),
        }
    });

    quote::quote! {
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
    }
}
