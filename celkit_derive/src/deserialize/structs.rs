use crate::attributes::ContainerAttributes;
use crate::utils::insert_trait_bounds;

use super::fields::generate_named_fields_deserialize;
use super::fields::generate_unnamed_fields_deserialize;
use super::fields::NamedFieldHandler;
use super::fields::UnnamedFieldHandler;

fn generate_named_struct_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsNamed,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
    let field_handler = NamedFieldHandler::new(fields, container_attributes)?;
    let field_names: Vec<_> = field_handler.field_names().collect();
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

    let construction = quote::quote! {
        Ok(#name {
            #(#field_names),*
        })
    };
    let context_name = format!("struct `{}`", name);
    let deserialization =
        generate_named_fields_deserialize(&field_handler, construction, &context_name);

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
                    ::celkit::core::Value::Struct(fields) => {
                        #deserialization
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
    let field_handler = UnnamedFieldHandler::new(fields)?;
    let field_idents = field_handler.field_idents();
    let construction = quote::quote! { Ok(#name(#(#field_idents),*)) };
    let context_name = format!("struct `{}`", name);
    let fields_format = quote::quote! { (_, field_value) };
    let deserialization = generate_unnamed_fields_deserialize(
        &field_handler,
        construction,
        &context_name,
        fields_format,
    );
    let generics = insert_trait_bounds(generics, "Deserialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Deserialize for #name #type_generics #where_clause {
            fn deserialize(value: ::celkit::core::Value) -> ::celkit::core::Result<Self> {
                match value {
                    ::celkit::core::Value::Struct(fields) => {
                        #deserialization
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
