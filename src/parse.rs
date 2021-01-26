use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, multispace0, multispace1, satisfy};
use nom::combinator::{opt, recognize};
use nom::error::{Error, ErrorKind};
use nom::multi::{many0, many1, separated_list0};
use nom::number::complete::double;
use nom::sequence::tuple;

use super::*;

#[cfg(test)]
mod test;

pub fn file(input: &str) -> IResult<(), Vec<RispExp>, RispErr> {
    let mut lists = many0(tuple((multispace0, list, multispace0)));

    match lists(input) {
        Ok(("", items)) => Ok(((), items.iter().map(|(_, item, _)| item.clone()).collect())),
        Ok((rest, _items)) => Err(nom::Err::Error(RispErr::Reason(format!(
            "Unknown text at end of file: {}",
            rest
        )))),
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(RispErr::Reason(format!(
            "parsing error: {:#?}",
            e.code
        )))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(RispErr::Reason(format!(
            "parsing failure: {:#?}",
            e.code
        )))),
        Err(nom::Err::Incomplete(needed)) => Err(nom::Err::Incomplete(needed)),
    }
}

pub fn list(input: &str) -> IResult<&str, RispExp> {
    let list_element = alt((bool, number, list, identifier));
    let mut list = tuple((
        multispace0,
        tag("("),
        multispace0,
        opt(separated_list0(multispace1, list_element)),
        multispace0,
        tag(")"),
        multispace0,
    ));

    let (rest, (_, _, _, items, _, _, _)) = list(input)?;

    if items.is_none() {
        return Ok((rest, RispExp::List(Vec::new())));
    }
    Ok((rest, RispExp::List(items.unwrap())))
}

pub fn bool(input: &str) -> IResult<&str, RispExp> {
    let mut truefalse = alt((tag("true"), tag("false")));

    let (rest, output) = truefalse(input)?;

    match output {
        "true" => Ok((rest, RispExp::Bool(true))),
        "false" => Ok((rest, RispExp::Bool(false))),
        _ => unreachable!(),
    }
}

pub fn number(input: &str) -> IResult<&str, RispExp> {
    alt((float_number, int_number))(input)
}

fn float_number(input: &str) -> IResult<&str, RispExp> {
    let (rest, float) = double(input)?;

    Ok((rest, RispExp::Number(float)))
}

fn int_number(input: &str) -> IResult<&str, RispExp> {
    let mut num = recognize(tuple((opt(alt((tag("+"), tag("-")))), digit1)));
    // (maybe + or -) then a number

    let (rest, num) = num(input)?;

    let integer: i64 = match num.parse() {
        Ok(it) => it,
        Err(_) => return Err(nom::Err::Error(Error::new(input, ErrorKind::Float))),
    };
    let float = integer as f64;

    Ok((rest, RispExp::Number(float)))
}

pub fn identifier(input: &str) -> IResult<&str, RispExp> {
    let mut identifier = recognize(many1(satisfy(|char| {
        !char.is_whitespace() && char != '(' && char != ')'
    })));
    // at least one character of anything but whitespace and parens

    let (rest, symbol) = identifier(input)?;

    return Ok((rest, RispExp::Symbol(symbol.to_string())));
}
