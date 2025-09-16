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
    pub(super) container_attributes: &'a ContainerAttributes,
}

impl<'a> NamedFieldHandler<'a> {
    pub fn new(
        fields: &'a syn::FieldsNamed,
        container_attributes: &'a ContainerAttributes,
    ) -> syn::Result<Self> {
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

        Ok(Self {
            fields,
            container_attributes,
        })
    }

    pub(super) fn generate_fields_deserialize(
        &self,
        construction: proc_macro2::TokenStream,
        context_name: &str,
    ) -> proc_macro2::TokenStream {
        let expected_fields_array = self.generate_expected_fields_array();
        let field_declarations = self.generate_field_declarations();
        let field_matching = self.generate_field_matching();
        let field_assignments = self.generate_field_assignments(context_name);
        let field_assignments_positional = self.generate_field_assignments_positional(context_name);

        quote::quote! {
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

                #(#field_assignments_positional)*

                return #construction;
            }

            #(#field_declarations)*

            // Unordered, we search for the values by names
            for (field_name, field_value) in fields {
                #(#field_matching)*
            }

            // Check whether all of the fields were found
            #(#field_assignments)*

            #construction
        }
    }

    pub(super) fn field_names(&self) -> Vec<&syn::Ident> {
        self.fields.iter().map(|field| field.name).collect()
    }

    fn generate_expected_fields_array(&self) -> proc_macro2::TokenStream {
        let field_names_str = self
            .fields
            .iter()
            .filter(|field| !field.attributes.skip && !field.attributes.skip_deserializing)
            .map(|field| {
                get_field_name(
                    &field.name.to_string(),
                    &field.attributes,
                    self.container_attributes,
                )
            });

        quote::quote! {
            let expected_fields = [#(#field_names_str),*];
        }
    }

    fn generate_field_declarations(&self) -> Vec<proc_macro2::TokenStream> {
        self.fields
            .iter()
            .map(|field| {
                let field_name = field.name;
                let field_type = field.ty;

                quote::quote! {
                    let mut #field_name: Option<#field_type> = None;
                }
            })
            .collect()
    }

    fn generate_field_matching(&self) -> Vec<proc_macro2::TokenStream> {
        self.fields
            .iter()
            .filter(|field| !field.attributes.skip && !field.attributes.skip_deserializing)
            .map(|field| {
                let field_name = field.name;
                let field_type = field.ty;
                let field_name_str = get_field_name(
                    &field_name.to_string(),
                    &field.attributes,
                    self.container_attributes,
                );
                let mut conditions = Vec::from([quote::quote! { field_name == #field_name_str }]);

                for alias in &field.attributes.alias {
                    conditions.push(quote::quote! { field_name == #alias });
                }

                quote::quote! {
                    if #(#conditions)||* {
                        #field_name = Some(<#field_type>::deserialize(field_value)?);

                        continue;
                    }
                }
            })
            .collect()
    }

    fn generate_field_assignments(&self, context_name: &str) -> Vec<proc_macro2::TokenStream> {
        let context = context_name.to_string();

        self.fields
            .iter()
            .map(move |field| {
                let field_name = field.name;
                let field_type = field.ty;

                if field.attributes.skip || field.attributes.skip_deserializing {
                    return quote::quote! {
                        let #field_name = <#field_type>::default();
                    };
                }

                if field.attributes.default {
                    return quote::quote! {
                        let #field_name = #field_name.unwrap_or_else(|| <#field_type>::default());
                    };
                }

                let field_name_str = get_field_name(
                    &field.name.to_string(),
                    &field.attributes,
                    self.container_attributes,
                );

                quote::quote! {
                    let #field_name = #field_name.ok_or_else(|| {
                        ::celkit::core::Error::new(format!(
                            "Missing field `{}` in {}",
                            #field_name_str,
                            #context,
                        ))
                    })?;
                }
            })
            .collect()
    }

    fn generate_field_assignments_positional(
        &self,
        context_name: &str,
    ) -> Vec<proc_macro2::TokenStream> {
        let context = context_name.to_string();

        self.fields
            .iter()
            .map(move |field| {
                let field_name = field.name;
                let field_type = field.ty;

                if field.attributes.skip || field.attributes.skip_deserializing {
                    return quote::quote! {
                        let #field_name = <#field_type>::default();
                    };
                }

                let field_name_str = get_field_name(
                    &field.name.to_string(),
                    &field.attributes,
                    self.container_attributes,
                );

                quote::quote! {
                    let #field_name = {
                        let (_, field_value) = fields_iter
                            .next()
                            .ok_or_else(|| ::celkit::core::Error::new(format!(
                                "Missing field `{}` in positional deserialization of {}",
                                #field_name_str,
                                #context,
                            )))?;

                        <#field_type>::deserialize(field_value)?
                    };
                }
            })
            .collect()
    }
}

pub(super) struct UnnamedField<'a> {
    pub(super) ty: &'a syn::Type,
    pub(super) attributes: FieldAttributes,
}

pub(super) struct UnnamedFieldHandler<'a> {
    pub(super) fields: Vec<UnnamedField<'a>>,
}

impl<'a> UnnamedFieldHandler<'a> {
    pub fn new(fields: &'a syn::FieldsUnnamed) -> syn::Result<Self> {
        let mut errors: Option<syn::Error> = None;
        let mut has_default_field = false;
        let mut handled_fields = Vec::new();

        for (i, field) in fields.unnamed.iter().enumerate() {
            let attributes = match parse_field_attributes(&field.attrs) {
                Ok(attributes) => attributes,
                Err(e) => {
                    match errors {
                        Some(ref mut errors) => errors.combine(e),
                        None => errors = Some(e),
                    }

                    continue;
                }
            };

            if has_default_field
                && !attributes.default
                && !attributes.skip
                && !attributes.skip_deserializing
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

            if attributes.default {
                has_default_field = true;
            }

            handled_fields.push(UnnamedField {
                ty: &field.ty,
                attributes,
            });
        }

        if let Some(e) = errors {
            return Err(e);
        }

        Ok(Self {
            fields: handled_fields,
        })
    }

    pub(super) fn generate_fields_deserialize(
        &self,
        construction: proc_macro2::TokenStream,
        context_name: &str,
        fields_format: proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        let field_count = self.field_count();
        let field_assignments = self.generate_field_assignments(context_name, fields_format);
        let context = context_name.to_string();

        quote::quote! {
            if fields.len() > #field_count {
                return Err(::celkit::core::Error::new(format!(
                    "Too many fields for {}, expected {}, got {}",
                    #context,
                    #field_count,
                    fields.len(),
                )));
            }

            let mut fields_iter = fields.into_iter();

            #(#field_assignments)*

            #construction
        }
    }

    pub(super) fn field_names(&self) -> Vec<syn::Ident> {
        (0..self.fields.len())
            .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()))
            .collect()
    }

    fn field_count(&self) -> usize {
        self.fields
            .iter()
            .filter(|field| !field.attributes.skip && !field.attributes.skip_deserializing)
            .count()
    }

    fn generate_field_assignments(
        &self,
        context_name: &str,
        fields_format: proc_macro2::TokenStream,
    ) -> Vec<proc_macro2::TokenStream> {
        let context = context_name.to_string();

        self.fields
            .iter()
            .enumerate()
            .map(move |(i, field)| {
                let field_type = field.ty;
                let field_attributes = &field.attributes;

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
                            Some(#fields_format) => <#field_type>::deserialize(field_value)?,
                            None => <#field_type>::default(),
                        };
                    };
                }

                quote::quote! {
                    let #field_ident = {
                        let #fields_format = fields_iter
                            .next()
                            .ok_or_else(|| ::celkit::core::Error::new(format!(
                                "Missing field {} in {}",
                                #i,
                                #context
                            )))?;

                        <#field_type>::deserialize(field_value)?
                    };
                }
            })
            .collect()
    }
}
