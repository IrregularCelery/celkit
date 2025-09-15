use crate::attributes::{ContainerAttributes, FieldAttributes, VariantAttributes};

pub(crate) fn insert_trait_bounds(mut generics: syn::Generics, trait_name: &str) -> syn::Generics {
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

pub(crate) fn unescape_identifier(identifier: &str) -> String {
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

pub(crate) fn get_field_name(
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

pub(crate) fn get_variant_name(
    variant_name: &str,
    variant_attributes: &VariantAttributes,
    container_attributes: &ContainerAttributes,
) -> String {
    if let Some(ref rename) = variant_attributes.rename {
        return rename.clone();
    }

    if let Some(ref rename_all) = container_attributes.rename_all {
        return rename_all.convert(variant_name);
    }

    variant_name.to_string()
}
