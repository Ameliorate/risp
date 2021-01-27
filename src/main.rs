use nom::IResult;
use std::fmt::Formatter;
use std::io;
use std::rc::Rc;

use parse::file;

pub mod types;
pub use types::*;

mod parse;

pub mod env;
pub use env::*;

fn parse(input: &str) -> Result<Vec<RispExp>, RispErr> {
    match file(input) {
        Ok(((), lists)) => Ok(lists),
        Err(nom::Err::Error(e)) => Err(e),
        Err(nom::Err::Failure(e)) => Err(e),
        Err(nom::Err::Incomplete(needed)) => {
            Err(RispErr::Reason(format!("incomplete parse: {:#?}", needed)))
        }
    }
}

/*
  Repl
*/

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let parsed_exp = parse(&expr)?;

    if parsed_exp.is_empty() {
        return Ok(RispExp::List(Vec::new()));
    }

    let mut evaled_exp: Option<RispExp> = None;

    for code in parsed_exp {
        evaled_exp = Some(env.eval(&code)?);
    }

    Ok(evaled_exp.unwrap())
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    let env = &mut RispEnv::default_env();
    loop {
        println!("risp >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("// 🔥 => {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("// 🙀 => {}", msg),
            },
        }
    }
}
