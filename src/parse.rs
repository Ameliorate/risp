use nom::branch::alt;
use nom::bytes::complete::{tag, is_not};
use nom::character::complete::{line_ending, multispace0, multispace1, not_line_ending, satisfy};
use nom::combinator::{eof, map, peek, recognize};
use nom::error::{context, convert_error, VerboseError};
use nom::multi::{many0, many1, separated_list0};
use nom::number::complete::double;
use nom::sequence::tuple;

use super::*;
use nom::lib::std::convert::identity;

pub fn file(input: &str) -> IResult<(), Vec<RispExp>, RispErr> {
    let mut lists = context("file", many0(tuple((multispace0, exp, multispace0))));

    match lists(input) {
        Ok(("", items)) => Ok((
            (),
            items
                .iter()
                .map(|(_, item, _)| item.clone())
                .filter_map(identity)
                .collect(),
        )),
        Ok((rest, _items)) => Err(nom::Err::Error(RispErr::Reason(format!(
            "unknown text at end of file: {}",
            rest
        )))),
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(RispErr::Reason(format!(
            "parsing error: {}",
            convert_error(input, e)
        )))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(RispErr::Reason(format!(
            "parsing failure: {}",
            convert_error(input, e)
        )))),
        Err(nom::Err::Incomplete(needed)) => Err(nom::Err::Incomplete(needed)),
    }
}

pub fn exp(input: &str) -> IResult<&str, Option<RispExp>, VerboseError<&str>> {
    context("exp", alt((comment, bool, number, string, list, identifier)))(input)
}

pub fn list(input: &str) -> IResult<&str, Option<RispExp>, VerboseError<&str>> {
    let mut list = context(
        "list",
        tuple((
            multispace0,
            context("list_open_paren", tag("(")),
            multispace0,
            context("list_contents", separated_list0(multispace1, exp)),
            multispace0,
            context("list_close_paren", tag(")")),
        )),
    );

    let (rest, (_, _, _, items, _, _)) = list(input)?;
    let items = items.iter().filter_map(|opt| opt.clone()).collect();

    Ok((rest, Some(RispExp::List(items))))
}

pub fn bool(input: &str) -> IResult<&str, Option<RispExp>, VerboseError<&str>> {
    let mut truefalse = context("boolean", alt((tag("true"), tag("false"))));

    let (rest, output) = truefalse(input)?;

    match output {
        "true" => Ok((rest, Some(RispExp::Bool(true)))),
        "false" => Ok((rest, Some(RispExp::Bool(false)))),
        _ => unreachable!(),
    }
}

pub fn number(input: &str) -> IResult<&str, Option<RispExp>, VerboseError<&str>> {
    context("number", map(double, |n| Option::Some(RispExp::Number(n))))(input)
}

pub fn identifier(input: &str) -> IResult<&str, Option<RispExp>, VerboseError<&str>> {
    let mut identifier = context(
        "identifier",
        recognize(many1(satisfy(|char| {
            !char.is_whitespace() && char != '(' && char != ')'
        }))),
    );
    // at least one character of anything but whitespace and parens

    let (rest, symbol) = identifier(input)?;

    return Ok((rest, Some(RispExp::Symbol(symbol.to_string()))));
}

pub fn comment(input: &str) -> IResult<&str, Option<RispExp>, VerboseError<&str>> {
    let mut comment = context(
        "comment",
        map(
            tuple((tag(";"), not_line_ending, peek(alt((line_ending, eof))))),
            |_| Option::None,
        ),
    );

    comment(input)
}

pub fn string(input: &str) -> IResult<&str, Option<RispExp>, VerboseError<&str>> {
    let double_string = tuple((tag("\""), is_not("\""), tag("\"")));
    let single_string = tuple((tag("\'"), is_not("\'"), tag("\'")));

    let mut string = context("string", map(alt((double_string, single_string)),
                                           |(_, contents, _): (&str, &str, &str)| Some(RispExp::String(contents.to_string()))));

    string(input)
}

#[cfg(test)]
mod test {
    use super::*;

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

        assert_eq!(
            result,
            vec![RispExp::List(vec![RispExp::Symbol("list".to_string())])]
        );
        Ok(())
    }

    #[test]
    fn parse_end_of_file_comment() -> Result<(), nom::Err<VerboseError<&'static str>>> {
        let (rest, result) = comment("; This is a test")?;

        assert_eq!(rest, "");
        assert_eq!(result, None);
        Ok(())
    }

    #[test]
    fn parse_single_string() -> Result<(), nom::Err<VerboseError<&'static str>>> {
        let (rest, result) = string("'This is \"a\" test'")?;

        assert_eq!(rest, "");
        assert_eq!(result, Some(RispExp::String("This is \"a\" test".to_string())));
        Ok(())
    }

    #[test]
    fn parse_double_string() -> Result<(), nom::Err<VerboseError<&'static str>>> {
        let (rest, result) = string("\"This is 'a' test\"")?;

        assert_eq!(rest, "");
        assert_eq!(result, Some(RispExp::String("This is 'a' test".to_string())));
        Ok(())
    }
}
