use crate::attributes::ContainerAttributes;
use crate::utils::insert_trait_bounds;

use super::fields::NamedFieldHandler;
use super::fields::UnnamedFieldHandler;

fn generate_named_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsNamed,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let field_handler = NamedFieldHandler::new(fields)?;
    let field_serialization =
        field_handler.generate_fields_serialize(container_attributes, quote::quote! { self. });
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                #field_serialization

                Ok(::celkit::core::Value::Struct(fields))
            }
        }
    })
}

fn generate_unnamed_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsUnnamed,
) -> syn::Result<proc_macro2::TokenStream> {
    let field_handler = UnnamedFieldHandler::new(fields)?;
    let field_serialization = field_handler.generate_fields_serialize(true);
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                #field_serialization

                Ok(::celkit::core::Value::Struct(fields))
            }
        }
    })
}

fn generate_unit_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
) -> syn::Result<proc_macro2::TokenStream> {
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                Ok(::celkit::core::Value::Struct(Vec::new()))
            }
        }
    })
}

pub(super) fn generate_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataStruct,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let fields = &data.fields;

    match fields {
        syn::Fields::Named(fields) => {
            generate_named_struct_serialize(name, generics, fields, container_attributes)
        }
        syn::Fields::Unnamed(fields) => generate_unnamed_struct_serialize(name, generics, fields),
        syn::Fields::Unit => generate_unit_struct_serialize(name, generics),
    }
}
