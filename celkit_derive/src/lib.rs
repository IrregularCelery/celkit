enum CaseStyle {
    Lower,
    Upper,
    Camel,
    Pascal,
    Snake,
    ScreamingSnake,
    Kebab,
    ScreamingKebab,
}

impl CaseStyle {
    const fn get_styles() -> [&'static str; 8] {
        [
            "lowercase",
            "UPPERCASE",
            "camelCase",
            "PascalCase",
            "snake_case",
            "SCREAMING_SNAKE_CASE",
            "kebab-case",
            "SCREAMING-KEBAB-CASE",
        ]
    }

    fn split_words(input: &str) -> Vec<String> {
        if input.is_empty() {
            return Vec::new();
        }

        let mut words = Vec::new();
        let mut current_word = String::new();
        let chars: Vec<char> = input.chars().collect();

        for (i, &c) in chars.iter().enumerate() {
            match c {
                '_' | '-' | ' ' => {
                    if !current_word.is_empty() {
                        words.push(current_word);

                        current_word = String::new();
                    }
                }
                c if c.is_uppercase() => {
                    if !current_word.is_empty() && (i == 0 || chars[i - 1].is_lowercase()) {
                        words.push(current_word);

                        current_word = String::new();
                    }

                    current_word.push(c.to_lowercase().next().unwrap_or(c));
                }
                c => {
                    current_word.push(c);
                }
            }
        }

        if !current_word.is_empty() {
            words.push(current_word);
        }

        words
    }

    fn from_str(input: &str) -> Option<Self> {
        match input {
            "lower" | "lowercase" => Some(CaseStyle::Lower),
            "upper" | "UPPERCASE" => Some(CaseStyle::Upper),
            "camel" | "camelCase" => Some(CaseStyle::Camel),
            "pascal" | "PascalCase" => Some(CaseStyle::Pascal),
            "snake" | "snake_case" => Some(CaseStyle::Snake),
            "screaming_snake" | "SCREAMING_SNAKE_CASE" => Some(CaseStyle::ScreamingSnake),
            "kebab" | "kebab-case" => Some(CaseStyle::Kebab),
            "screaming_kebab" | "SCREAMING-KEBAB-CASE" => Some(CaseStyle::ScreamingKebab),
            _ => None,
        }
    }

    fn to_lower_case(&self, input: &str) -> String {
        input.to_lowercase()
    }

    fn to_upper_case(&self, input: &str) -> String {
        input.to_uppercase()
    }

    fn to_camel_case(&self, input: &str) -> String {
        let words = Self::split_words(input);

        if words.is_empty() {
            return String::new();
        }

        let mut output = String::with_capacity(input.len());

        // First word is lowercase
        output.push_str(&words[0].to_lowercase());

        // The rest of the words are capitalized
        for word in &words[1..] {
            if let Some(first_char) = word.chars().next() {
                output.push(first_char.to_uppercase().next().unwrap_or(first_char));
                output.push_str(&word[1..].to_lowercase());
            }
        }

        output
    }

    fn to_pascal_case(&self, input: &str) -> String {
        let words = Self::split_words(input);
        let mut output = String::with_capacity(input.len());

        // All words are capitalized
        for word in words {
            if let Some(first_char) = word.chars().next() {
                output.push(first_char.to_uppercase().next().unwrap_or(first_char));
                output.push_str(&word[1..].to_lowercase());
            }
        }

        output
    }

    fn to_snake_case(&self, input: &str) -> String {
        let words = Self::split_words(input);

        words.join("_").to_lowercase()
    }

    fn to_screaming_snake_case(&self, input: &str) -> String {
        let words = Self::split_words(input);

        words.join("_").to_uppercase()
    }

    fn to_kebab_case(&self, input: &str) -> String {
        let words = Self::split_words(input);

        words.join("-").to_lowercase()
    }

    fn to_screaming_kebab_case(&self, input: &str) -> String {
        let words = Self::split_words(input);

        words.join("-").to_uppercase()
    }

    fn convert(&self, input: &str) -> String {
        match self {
            CaseStyle::Lower => self.to_lower_case(input),
            CaseStyle::Upper => self.to_upper_case(input),
            CaseStyle::Camel => self.to_camel_case(input),
            CaseStyle::Pascal => self.to_pascal_case(input),
            CaseStyle::Snake => self.to_snake_case(input),
            CaseStyle::ScreamingSnake => self.to_screaming_snake_case(input),
            CaseStyle::Kebab => self.to_kebab_case(input),
            CaseStyle::ScreamingKebab => self.to_screaming_kebab_case(input),
        }
    }
}

#[derive(Default)]
struct ContainerAttributes {
    rename_all: Option<CaseStyle>,
}

#[derive(Default)]
struct FieldAttributes {
    rename: Option<String>,
    alias: Vec<String>,
    default: bool,
    skip: bool,
    skip_serializing: bool,
    skip_deserializing: bool,
}

#[proc_macro_derive(Serialize, attributes(celkit))]
pub fn derive_serialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &input.ident;
    let generics = input.generics;
    let attributes = &input.attrs;

    let container_attributes = match parse_container_attributes(attributes) {
        Ok(attributes) => attributes,
        Err(e) => return e.to_compile_error().into(),
    };

    let impl_serialize = match &input.data {
        syn::Data::Struct(data) => {
            generate_struct_serialize(name, generics, data, &container_attributes)
        }
        syn::Data::Enum(data) => generate_enum_serialize(name, generics, data),
        syn::Data::Union(_) => Err(syn::Error::new_spanned(
            name,
            "Serialization isn't available for unions",
        )),
    };

    let tokens = match impl_serialize {
        Ok(tokens) => tokens,
        Err(e) => return e.to_compile_error().into(),
    };

    proc_macro::TokenStream::from(tokens)
}

#[proc_macro_derive(Deserialize, attributes(celkit))]
pub fn derive_deserialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &input.ident;
    let generics = input.generics;
    let attributes = &input.attrs;

    let container_attributes = match parse_container_attributes(attributes) {
        Ok(attributes) => attributes,
        Err(e) => return e.to_compile_error().into(),
    };

    let impl_deserialize = match &input.data {
        syn::Data::Struct(data) => {
            generate_struct_deserialize(name, generics, data, &container_attributes)
        }
        syn::Data::Enum(data) => generate_enum_deserialize(name, generics, data),
        syn::Data::Union(_) => {
            return syn::Error::new_spanned(name, "Serialization isn't available for unions")
                .to_compile_error()
                .into();
        }
    };

    let tokens = match impl_deserialize {
        Ok(tokens) => tokens,
        Err(e) => return e.to_compile_error().into(),
    };

    proc_macro::TokenStream::from(tokens)
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

fn unescape_identifier(identifier: &str) -> String {
    if !identifier.starts_with("r#") {
        return identifier.to_string();
    }

    let unescaped = &identifier[2..];

    match unescaped {
        "abstract" | "as" | "async" | "await" | "become" | "box" | "break" | "const"
        | "continue" | "crate" | "do" | "dyn" | "else" | "enum" | "extern" | "false" | "final"
        | "fn" | "for" | "gen" | "if" | "impl" | "in" | "let" | "loop" | "macro" | "match"
        | "mod" | "move" | "mut" | "override" | "priv" | "pub" | "ref" | "return" | "static"
        | "struct" | "super" | "trait" | "true" | "try" | "type" | "typeof" | "unsafe"
        | "unsized" | "use" | "virtual" | "where" | "while" | "yield" => unescaped.to_string(),
        _ => identifier.to_string(),
    }
}

fn parse_container_attributes(attributes: &[syn::Attribute]) -> syn::Result<ContainerAttributes> {
    let mut container_attributes = ContainerAttributes::default();

    for attribute in attributes {
        if !attribute.path().is_ident("celkit") {
            continue;
        }

        attribute.parse_nested_meta(|meta| {
            if let Some(path) = meta.path.get_ident() {
                return match path.to_string().as_str() {
                    "rename_all" => {
                        let value = meta.value()?;
                        let s: syn::LitStr = value.parse()?;
                        let case_style = CaseStyle::from_str(&s.value());

                        if case_style.is_none() {
                            return Err(meta.error(format!(
                                "Unknown case style, Available styles: {}",
                                CaseStyle::get_styles().join(", ")
                            )));
                        }

                        container_attributes.rename_all = case_style;

                        Ok(())
                    }
                    unknown => Err(meta.error(format!(
                        "Unknown `celkit` container attribute `{}`",
                        unknown
                    ))),
                };
            }

            Err(meta.error(format!(
                "Unknown `celkit` container attribute `{}`",
                quote::ToTokens::to_token_stream(&meta.path)
                    .to_string()
                    .replace(' ', "")
            )))
        })?;
    }

    Ok(container_attributes)
}

fn parse_field_attributes(attributes: &[syn::Attribute]) -> syn::Result<FieldAttributes> {
    let mut field_attributes = FieldAttributes::default();

    for attribute in attributes {
        if !attribute.path().is_ident("celkit") {
            continue;
        }

        attribute.parse_nested_meta(|meta| {
            if let Some(path) = meta.path.get_ident() {
                return match path.to_string().as_str() {
                    "rename" => {
                        let value = meta.value()?;
                        let s: syn::LitStr = value.parse()?;

                        field_attributes.rename = Some(s.value());

                        Ok(())
                    }
                    "alias" => {
                        let value = meta.value()?;
                        let s: syn::LitStr = value.parse()?;

                        field_attributes.alias.push(s.value());

                        Ok(())
                    }
                    "default" => {
                        field_attributes.default = true;

                        Ok(())
                    }
                    "skip" => {
                        field_attributes.skip = true;

                        Ok(())
                    }
                    "skip_serializing" => {
                        field_attributes.skip_serializing = true;

                        Ok(())
                    }
                    "skip_deserializing" => {
                        field_attributes.skip_deserializing = true;

                        Ok(())
                    }
                    unknown => {
                        Err(meta.error(format!("Unknown `celkit` field attribute `{}`", unknown)))
                    }
                };
            }

            Err(meta.error(format!(
                "Unknown `celkit` field attribute `{}`",
                quote::ToTokens::to_token_stream(&meta.path)
                    .to_string()
                    .replace(' ', "")
            )))
        })?;
    }

    Ok(field_attributes)
}

fn get_field_name(
    field_name: &str,
    field_attributes: &FieldAttributes,
    container_attributes: &ContainerAttributes,
) -> String {
    if let Some(ref rename) = field_attributes.rename {
        return rename.clone();
    }

    let unescaped = unescape_identifier(field_name);

    if let Some(ref rename_all) = container_attributes.rename_all {
        return rename_all.convert(&unescaped);
    }

    unescaped
}

// ------------------------ Struct Serialization -------------------------- //

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

fn generate_struct_serialize(
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

// ----------------------- Struct Deserialization ------------------------- //

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

fn generate_struct_deserialize(
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

// ------------------------- Enum Serialization --------------------------- //

fn generate_named_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    fields: &syn::FieldsNamed,
    container_attributes: &ContainerAttributes,
) -> syn::Result<proc_macro2::TokenStream> {
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
            container_attributes,
        );

        quote::quote! {
            fields.push((
                #field_name_str.to_string(),
                #field_name.serialize()?
            ));
        }
    });

    Ok(quote::quote! {
        #name::#variant_name { #(#field_names),* } => {
            use ::celkit::core::*;

            let mut fields = Vec::with_capacity(#field_count);

            #(#fields)*

            let variant_value = ::celkit::core::Value::Struct(fields);
            let mut variant_object = BTreeMap::new();

            variant_object.insert(stringify!(#variant_name).to_string(), variant_value);

            Ok(::celkit::core::Value::Object(variant_object))
        }
    })
}

fn generate_unnamed_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
    fields: &syn::FieldsUnnamed,
) -> syn::Result<proc_macro2::TokenStream> {
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

    Ok(quote::quote! {
        #name::#variant_name(#(#field_names),*) => {
            use ::celkit::core::*;

            let mut fields = Vec::with_capacity(#field_count);

            #(#fields)*

            let variant_value = ::celkit::core::Value::Tuple(fields);
            let mut variant_object = BTreeMap::new();

            variant_object.insert(stringify!(#variant_name).to_string(), variant_value);

            Ok(::celkit::core::Value::Object(variant_object))
        }
    })
}

fn generate_unit_enum_serialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
) -> syn::Result<proc_macro2::TokenStream> {
    Ok(quote::quote! {
        #name::#variant_name => {
            use ::celkit::core::*;

            let mut variant_object = BTreeMap::new();

            variant_object.insert(stringify!(#variant_name).to_string(), ::celkit::core::Value::Null);

            Ok(::celkit::core::Value::Object(variant_object))
        }
    })
}

fn generate_enum_serialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataEnum,
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
            // TODO: Should be parsed as variant_attributes
            let container_attributes = parse_container_attributes(attributes)?;

            match &variant.fields {
                syn::Fields::Named(fields) => {
                    generate_named_enum_serialize(name, variant_name, fields, &container_attributes)
                }
                syn::Fields::Unnamed(fields) => {
                    generate_unnamed_enum_serialize(name, variant_name, fields)
                }
                syn::Fields::Unit => generate_unit_enum_serialize(name, variant_name),
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

// ------------------------ Enum Deserialization -------------------------- //

fn generate_named_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
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

    if field_names.is_empty() {
        return Ok(quote::quote! {
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
                        "Missing field `{}` in variant `{}`",
                        #field_name_str,
                        stringify!(#variant_name),
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
                            "Missing field `{}` in positional deserialization of variant `{}`",
                            #field_name_str,
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

    Ok(quote::quote! {
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
                    stringify!(#variant_name),
                )))
            }
        }
    })
}

fn generate_unnamed_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
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
                            stringify!(#variant_name),
                        )))?
                )?;
            }
        });
    let field_idents = (0..field_types.len())
        .map(|i| syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site()));

    Ok(quote::quote! {
        stringify!(#variant_name) => {
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
                    stringify!(#variant_name),
                )))
            }
        }
    })
}

fn generate_unit_enum_deserialize(
    name: &syn::Ident,
    variant_name: &syn::Ident,
) -> syn::Result<proc_macro2::TokenStream> {
    Ok(quote::quote! {
        stringify!(#variant_name) => {
            match variant_value {
                ::celkit::core::Value::Null => Ok(#name::#variant_name),
                _ => Err(::celkit::core::Error::new(format!(
                    "Expected `null` for enum variant `{}`",
                    stringify!(#variant_name),
                )))
            }
        }
    })
}

fn generate_enum_deserialize(
    name: &syn::Ident,
    generics: syn::Generics,
    data: &syn::DataEnum,
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
            // TODO: Should be parsed as variant_attributes
            let container_attributes = parse_container_attributes(attributes)?;

            match &variant.fields {
                syn::Fields::Named(fields) => generate_named_enum_deserialize(
                    name,
                    variant_name,
                    fields,
                    &container_attributes,
                ),
                syn::Fields::Unnamed(fields) => {
                    generate_unnamed_enum_deserialize(name, variant_name, fields)
                }
                syn::Fields::Unit => generate_unit_enum_deserialize(name, variant_name),
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
