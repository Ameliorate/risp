use super::*;

/*
 * Parse
 */

#[test]
fn parse_bool_true() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = bool("true")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::Bool(true)));
    Ok(())
}

#[test]
fn parse_bool_false() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = bool("false")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::Bool(false)));
    Ok(())
}

#[test]
fn parse_number_float() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = number("12.1")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::Number(12.1)));
    Ok(())
}

#[test]
fn parse_number_int() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = number("12")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::Number(12.0)));
    Ok(())
}

#[test]
fn parse_identifier_alpha() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = identifier("is")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::Symbol("is".to_string())));
    Ok(())
}

#[test]
fn parse_identifier_symbol() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = identifier("!")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::Symbol("!".to_string())));
    Ok(())
}

#[test]
fn parse_identifier_spaces() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = identifier("test rest")?;

    assert_eq!(rest, " rest");
    assert_eq!(result, Some(RispExp::Symbol("test".to_string())));
    Ok(())
}

#[test]
fn parse_list_empty() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = list("()")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::List(Vec::new())));
    Ok(())
}

#[test]
fn parse_list_numbers() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = list("(12 13.2 100)")?;

    assert_eq!(rest, "");
    assert_eq!(
        result,
        Some(RispExp::List(vec![
            RispExp::Number(12.0),
            RispExp::Number(13.2),
            RispExp::Number(100.0)
        ]))
    );
    Ok(())
}

#[test]
fn parse_list_empty_spaces() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = list("( )")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::List(Vec::new())));
    Ok(())
}

#[test]
fn parse_list_empty_newline() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = list("(\n)")?;

    assert_eq!(rest, "");
    assert_eq!(result, Some(RispExp::List(Vec::new())));
    Ok(())
}

#[test]
fn parse_list_spaces_after() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = list("() ")?;

    assert_eq!(rest, " ");
    assert_eq!(result, Some(RispExp::List(Vec::new())));
    Ok(())
}

#[test]
fn parse_list_nested() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = list("(test a (plus 1 2))")?;

    assert_eq!(rest, "");
    assert_eq!(
        result,
        Some(RispExp::List(vec![
            RispExp::Symbol("test".to_string()),
            RispExp::Symbol("a".to_string()),
            RispExp::List(vec![
                RispExp::Symbol("plus".to_string()),
                RispExp::Number(1.0),
                RispExp::Number(2.0)
            ])
        ]))
    );
    Ok(())
}

#[test]
fn parse_list_multi_nested() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = list("(() ())")?;

    assert_eq!(
        result,
        Some(RispExp::List(vec![
            RispExp::List(Vec::new()),
            RispExp::List(Vec::new())
        ]))
    );
    assert_eq!(rest, "");
    Ok(())
}

#[test]
fn parse_file() -> Result<(), nom::Err<RispErr>> {
    let ((), result) = file("(test)\n(test2)")?;

    assert_eq!(
        result,
        vec![
            RispExp::List(vec![RispExp::Symbol("test".to_string())]),
            RispExp::List(vec![RispExp::Symbol("test2".to_string())])
        ]
    );
    Ok(())
}

#[test]
fn parse_comment() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = comment("; This is a test\n")?;

    assert_eq!(rest, "\n");
    assert_eq!(result, None);
    Ok(())
}

#[test]
fn parse_comment_in_list() -> Result<(), nom::Err<VerboseError<&'static str>>> {
    let (rest, result) = list("(test ; This is a test\n)")?;

    assert_eq!(rest, "");
    assert_eq!(
        result,
        Some(RispExp::List(vec![RispExp::Symbol("test".to_string())]))
    );
    Ok(())
}

#[test]
fn parse_file_comment() -> Result<(), nom::Err<RispErr>> {
    let ((), result) = file("; This is a comment\n")?;

    assert_eq!(result, Vec::new());
    Ok(())
}

#[test]
fn parse_file_list_comment() -> Result<(), nom::Err<RispErr>> {
    let ((), result) = file("(list); This is a comment\n")?;

    assert_eq!(result, vec![RispExp::List(vec![RispExp::Symbol("list".to_string())])]);
    Ok(())
}
