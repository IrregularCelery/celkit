// Helper function to strip r# from Rust reserved keywords
#[doc(hidden)]
pub fn unescape_identifier(identifier: &str) -> &str {
    if !identifier.starts_with("r#") {
        return identifier;
    }

    let unescaped = &identifier[2..];

    match unescaped {
        "abstract" | "as" | "async" | "await" | "become" | "box" | "break" | "const"
        | "continue" | "crate" | "do" | "dyn" | "else" | "enum" | "extern" | "false" | "final"
        | "fn" | "for" | "gen" | "if" | "impl" | "in" | "let" | "loop" | "macro" | "match"
        | "mod" | "move" | "mut" | "override" | "priv" | "pub" | "ref" | "return" | "static"
        | "struct" | "super" | "trait" | "true" | "try" | "type" | "typeof" | "unsafe"
        | "unsized" | "use" | "virtual" | "where" | "while" | "yield" => unescaped,
        _ => identifier,
    }
}
