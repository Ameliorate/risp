use super::*;

/*
 * Parse
 */

#[test]
fn parse_bool_true() {
    let (rest, result) = bool("true").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::Bool(true));
}

#[test]
fn parse_bool_false() {
    let (rest, result) = bool("false").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::Bool(false));
}

#[test]
fn parse_number_float() {
    let (rest, result) = number("12.1").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::Number(12.1));
}

#[test]
fn parse_number_int() {
    let (rest, result) = number("12").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::Number(12.0));
}

#[test]
fn parse_identifier_alpha() {
    let (rest, result) = identifier("is").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::Symbol("is".to_string()));
}

#[test]
fn parse_identifier_symbol() {
    let (rest, result) = identifier("!").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::Symbol("!".to_string()));
}

#[test]
fn parse_identifier_spaces() {
    let (rest, result) = identifier("test rest").unwrap();

    assert_eq!(rest, " rest");
    assert_eq!(result, RispExp::Symbol("test".to_string()));
}

#[test]
fn parse_list_empty() {
    let (rest, result) = list("()").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::List(Vec::new()));
}

#[test]
fn parse_list_numbers() {
    let (rest, result) = list("(12 13.2 100)").unwrap();

    assert_eq!(rest, "");
    assert_eq!(
        result,
        RispExp::List(vec![
            RispExp::Number(12.0),
            RispExp::Number(13.2),
            RispExp::Number(100.0)
        ])
    );
}

#[test]
fn parse_list_empty_spaces() {
    let (rest, result) = list("( )").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::List(Vec::new()));
}

#[test]
fn parse_list_empty_newline() {
    let (rest, result) = list("(\n)").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::List(Vec::new()));
}

#[test]
fn parse_list_spaces_after() {
    let (rest, result) = list("() ").unwrap();

    assert_eq!(rest, "");
    assert_eq!(result, RispExp::List(Vec::new()));
}

#[test]
fn parse_list_nested() {
    let (rest, result) = list("(test a (plus 1 2))").unwrap();

    assert_eq!(rest, "");
    assert_eq!(
        result,
        RispExp::List(vec![
            RispExp::Symbol("test".to_string()),
            RispExp::Symbol("a".to_string()),
            RispExp::List(vec![
                RispExp::Symbol("plus".to_string()),
                RispExp::Number(1.0),
                RispExp::Number(2.0)
            ])
        ])
    );
}

#[test]
fn parse_list_multi_nested() {
    let (rest, result) = list("(() ())").unwrap();

    assert_eq!(
        result,
        RispExp::List(vec![RispExp::List(Vec::new()), RispExp::List(Vec::new())])
    );
    assert_eq!(rest, "")
}

#[test]
fn parse_file() {
    let ((), result) = file("(test)\n(test2)").unwrap();

    assert_eq!(
        result,
        vec![
            RispExp::List(vec![RispExp::Symbol("test".to_string())]),
            RispExp::List(vec![RispExp::Symbol("test2".to_string())])
        ]
    )
}
