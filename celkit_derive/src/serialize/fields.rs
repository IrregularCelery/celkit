use crate::attributes::parse_field_attributes;
use crate::attributes::{ContainerAttributes, FieldAttributes};
use crate::utils::get_field_name;

pub(super) struct NamedField<'a> {
    pub(super) name: &'a syn::Ident,
    pub(super) ty: &'a syn::Type,
    pub(super) attributes: FieldAttributes,
}

pub(super) struct NamedFieldHandler<'a> {
    pub(super) fields: Vec<NamedField<'a>>,
}

impl<'a> NamedFieldHandler<'a> {
    pub fn new(fields: &'a syn::FieldsNamed) -> syn::Result<Self> {
        let fields = fields
            .named
            .iter()
            .filter_map(|field| match field.ident.as_ref() {
                Some(name) => {
                    Some(
                        parse_field_attributes(&field.attrs).map(|attributes| NamedField {
                            name,
                            ty: &field.ty,
                            attributes,
                        }),
                    )
                }
                None => None,
            })
            .collect::<syn::Result<Vec<_>>>()?;

        Ok(Self { fields })
    }

    pub(super) fn generate_fields_serialize(
        &self,
        container_attributes: &'a ContainerAttributes,
        root_format: proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        let fields: Vec<_> = self
            .serializable_fields()
            .map(|field| {
                let field_name = field.name;
                let field_name_str = get_field_name(
                    &field_name.to_string(),
                    &field.attributes,
                    container_attributes,
                );

                quote::quote! {
                    fields.push((
                        #field_name_str.to_string(),
                        #root_format #field_name.serialize()?
                    ));
                }
            })
            .collect();
        let field_count = self.field_count();

        quote::quote! {
            use ::celkit::core::*;

            let mut fields = Vec::with_capacity(#field_count);

            #(#fields)*
        }
    }

    pub(super) fn field_names(&self) -> Vec<&syn::Ident> {
        self.fields.iter().map(|field| field.name).collect()
    }

    fn serializable_fields(&self) -> impl Iterator<Item = &NamedField> {
        self.fields
            .iter()
            .filter(|field| !field.attributes.skip && !field.attributes.skip_serializing)
    }

    fn field_count(&self) -> usize {
        self.serializable_fields().count()
    }
}
