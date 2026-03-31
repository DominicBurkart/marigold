use marigold_grammar::parser::parse_marigold;

#[test]
fn test_parse_empty_input() {
    // Empty input is treated as a valid (no-op) program by the parser.
    let result = parse_marigold("");
    assert!(
        result.is_ok(),
        "Empty input should parse successfully, got: {:?}",
        result,
    );
}

#[test]
fn test_parse_invalid_syntax() {
    // Gibberish that doesn't match any grammar rule should produce an error.
    let result = parse_marigold("!@#$%^&*() this is not valid marigold");
    assert!(
        result.is_err(),
        "Invalid syntax should produce a parse error"
    );
    let err = result.unwrap_err();
    let msg = format!("{}", err);
    assert!(!msg.is_empty(), "Error message should not be empty");
}
