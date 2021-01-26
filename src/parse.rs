use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{multispace0, multispace1, satisfy};
use nom::combinator::{map, recognize};
use nom::error::{VerboseError, convert_error, context};
use nom::multi::{many0, many1, separated_list0};
use nom::number::complete::double;
use nom::sequence::tuple;

use super::*;

#[cfg(test)]
mod test;

pub fn file(input: &str) -> IResult<(), Vec<RispExp>, RispErr> {
    let mut lists = context("file", many0(tuple((multispace0, list, multispace0))));

    match lists(input) {
        Ok(("", items)) => Ok(((), items.iter().map(|(_, item, _)| item.clone()).collect())),
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

pub fn list(input: &str) -> IResult<&str, RispExp, VerboseError<&str>> {
    let list_element = context("list_element", alt((bool, number, list, identifier)));
    let mut list = context("list", tuple((
        multispace0,
        tag("("),
        multispace0,
        separated_list0(multispace1, list_element),
        multispace0,
        tag(")"),
        multispace0,
    )));

    let (rest, (_, _, _, items, _, _, _)) = list(input)?;

    Ok((rest, RispExp::List(items)))
}

pub fn bool(input: &str) -> IResult<&str, RispExp, VerboseError<&str>> {
    let mut truefalse = context("boolean", alt((tag("true"), tag("false"))));

    let (rest, output) = truefalse(input)?;

    match output {
        "true" => Ok((rest, RispExp::Bool(true))),
        "false" => Ok((rest, RispExp::Bool(false))),
        _ => unreachable!(),
    }
}

pub fn number(input: &str) -> IResult<&str, RispExp, VerboseError<&str>> {
    context("number", map(double, RispExp::Number))(input)
}

pub fn identifier(input: &str) -> IResult<&str, RispExp, VerboseError<&str>> {
    let mut identifier = context("identifier", recognize(many1(satisfy(|char| {
        !char.is_whitespace() && char != '(' && char != ')'
    }))));
    // at least one character of anything but whitespace and parens

    let (rest, symbol) = identifier(input)?;

    return Ok((rest, RispExp::Symbol(symbol.to_string())));
}
