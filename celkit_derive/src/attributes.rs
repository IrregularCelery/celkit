pub(crate) enum CaseStyle {
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

    pub(crate) fn convert(&self, input: &str) -> String {
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
pub(crate) struct ContainerAttributes {
    pub(crate) rename_all: Option<CaseStyle>,
}

#[derive(Default)]
pub(crate) struct FieldAttributes {
    pub(crate) rename: Option<String>,
    pub(crate) alias: Vec<String>,
    pub(crate) default: bool,
    pub(crate) skip: bool,
    pub(crate) skip_serializing: bool,
    pub(crate) skip_deserializing: bool,
}

#[derive(Default)]
pub(crate) struct VariantAttributes {
    // Each variant itself is a container for its fields
    pub(crate) container: ContainerAttributes,
    pub(crate) rename: Option<String>,
    pub(crate) alias: Vec<String>,
    pub(crate) skip: bool,
    pub(crate) skip_serializing: bool,
    pub(crate) skip_deserializing: bool,
}

pub(crate) fn parse_container_attributes(
    attributes: &[syn::Attribute],
) -> syn::Result<ContainerAttributes> {
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

pub(crate) fn parse_field_attributes(
    attributes: &[syn::Attribute],
) -> syn::Result<FieldAttributes> {
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

pub(crate) fn parse_variant_attributes(
    attributes: &[syn::Attribute],
) -> syn::Result<VariantAttributes> {
    let mut variant_attributes = VariantAttributes::default();

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

                        variant_attributes.container.rename_all = case_style;

                        Ok(())
                    }
                    "rename" => {
                        let value = meta.value()?;
                        let s: syn::LitStr = value.parse()?;

                        variant_attributes.rename = Some(s.value());

                        Ok(())
                    }
                    "alias" => {
                        let value = meta.value()?;
                        let s: syn::LitStr = value.parse()?;

                        variant_attributes.alias.push(s.value());

                        Ok(())
                    }
                    "skip" => {
                        variant_attributes.skip = true;

                        Ok(())
                    }
                    "skip_serializing" => {
                        variant_attributes.skip_serializing = true;

                        Ok(())
                    }
                    "skip_deserializing" => {
                        variant_attributes.skip_deserializing = true;

                        Ok(())
                    }
                    unknown => {
                        Err(meta.error(format!("Unknown `celkit` variant attribute `{}`", unknown)))
                    }
                };
            }

            Err(meta.error(format!(
                "Unknown `celkit` variant attribute `{}`",
                quote::ToTokens::to_token_stream(&meta.path)
                    .to_string()
                    .replace(' ', "")
            )))
        })?;
    }

    Ok(variant_attributes)
}
