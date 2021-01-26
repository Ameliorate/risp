use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, multispace0, multispace1, not_line_ending, satisfy};
use nom::combinator::{map, peek, recognize};
use nom::error::{context, convert_error, VerboseError};
use nom::multi::{many0, many1, separated_list0};
use nom::number::complete::double;
use nom::sequence::tuple;

use super::*;
use nom::lib::std::convert::identity;

#[cfg(test)]
mod test;

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
    context("exp", alt((comment, bool, number, list, identifier)))(input)
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
            tuple((tag(";"), not_line_ending, peek(line_ending))),
            |_| Option::None,
        ),
    );

    comment(input)
}
