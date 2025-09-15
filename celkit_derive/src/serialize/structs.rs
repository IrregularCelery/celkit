use crate::attributes::parse_field_attributes;
use crate::attributes::ContainerAttributes;
use crate::utils::{get_field_name, insert_trait_bounds};

fn generate_named_struct_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    fields: &syn::FieldsNamed,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
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
            container_attributes,
        );

        quote::quote! {
            fields.push((
                #field_name_str.to_string(),
                self.#field_name.serialize()?
            ));
        }
    });
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                let mut fields = Vec::with_capacity(#field_count);

                #(#fields)*

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
        let index = syn::Index::from(*i);

        quote::quote! {
            fields.push((String::new(), self.#index.serialize()?));
        }
    });
    let generics = insert_trait_bounds(generics, "Serialize");
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(quote::quote! {
        impl #impl_generics ::celkit::Serialize for #name #type_generics #where_clause {
            fn serialize(&self) -> ::celkit::core::Result<::celkit::core::Value> {
                use ::celkit::core::*;

                let mut fields = Vec::with_capacity(#field_count);

                #(#fields)*

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
