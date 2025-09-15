use crate::attributes::{parse_field_attributes, parse_variant_attributes};
use crate::attributes::{ContainerAttributes, VariantAttributes};
use crate::utils::{get_field_name, get_variant_name, insert_trait_bounds};

fn generate_named_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
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

    let field_names: Vec<_> = fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .collect();
    let serializable_fields: Vec<_> = fields
        .named
        .iter()
        .map(|field| parse_field_attributes(&field.attrs).map(|attributes| (field, attributes)))
        .collect::<syn::Result<Vec<_>>>()?
        .into_iter()
        .filter(|(_, attributes)| !attributes.skip && !attributes.skip_serializing)
        .collect();
    let field_count = serializable_fields.len();
    let fields = serializable_fields.iter().map(|(field, field_attributes)| {
        let Some(field_name) = &field.ident else {
            return syn::Error::new_spanned(field, "Named fields must have an `identifier`")
                .to_compile_error();
        };

        let field_name_str = get_field_name(
            &field_name.to_string(),
            field_attributes,
            &variant_attributes.container,
        );

        quote::quote! {
            fields.push((
                #field_name_str.to_string(),
                #field_name.serialize()?
            ));
        }
    });
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );

    Ok(quote::quote! {
        #name::#variant_name { #(#field_names),* } => {
            use ::celkit::core::*;

            let mut fields = Vec::with_capacity(#field_count);

            #(#fields)*

            let variant_value = ::celkit::core::Value::Struct(fields);
            let mut variant_object = BTreeMap::new();

            variant_object.insert(#variant_name_str.to_string(), variant_value);

            Ok(::celkit::core::Value::Object(variant_object))
        }
    })
}

fn generate_unnamed_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
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

    let field_names: Vec<_> = (0..fields.unnamed.len())
        .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()))
        .collect();
    let serializable_fields: Vec<_> = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(i, field)| {
            parse_field_attributes(&field.attrs).map(|attributes| (i, field, attributes))
        })
        .collect::<syn::Result<Vec<_>>>()?
        .into_iter()
        .filter(|(_, _, attributes)| !attributes.skip && !attributes.skip_serializing)
        .collect();
    let field_count = serializable_fields.len();
    let fields = serializable_fields.iter().map(|(i, _, _)| {
        let field_name = &field_names[*i];

        quote::quote! {
            fields.push(#field_name.serialize()?);
        }
    });
    let variant_name_str = get_variant_name(
        &variant_name.to_string(),
        &variant_attributes,
        container_attributes,
    );

    Ok(quote::quote! {
        #name::#variant_name(#(#field_names),*) => {
            use ::celkit::core::*;

            let mut fields = Vec::with_capacity(#field_count);

            #(#fields)*

            let variant_value = ::celkit::core::Value::Tuple(fields);
            let mut variant_object = BTreeMap::new();

            variant_object.insert(#variant_name_str.to_string(), variant_value);

            Ok(::celkit::core::Value::Object(variant_object))
        }
    })
}

pub(super) fn generate_unit_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
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

            let mut variant_object = BTreeMap::new();

            variant_object.insert(#variant_name_str.to_string(), ::celkit::core::Value::Null);

            Ok(::celkit::core::Value::Object(variant_object))
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
        .map(|variant| {
            let variant_name = &variant.ident;
            let attributes = &variant.attrs;
            let variant_attributes = parse_variant_attributes(attributes)?;

            match &variant.fields {
                syn::Fields::Named(fields) => generate_named_enum_serialize(
                    name,
                    variant_name,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                ),
                syn::Fields::Unnamed(fields) => generate_unnamed_enum_serialize(
                    name,
                    variant_name,
                    fields,
                    &container_attributes,
                    &variant_attributes,
                ),
                syn::Fields::Unit => generate_unit_enum_serialize(
                    name,
                    variant_name,
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
