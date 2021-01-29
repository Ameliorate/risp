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

pub fn parse(input: &str) -> Result<Vec<RispExp>, RispErr> {
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
        match env.parse_eval(expr) {
            Ok(res) => println!("// 🔥 => {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("// 🙀 => {}", msg),
            },
        }
    }
}
