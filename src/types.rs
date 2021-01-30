use nom::lib::std::cmp::Ordering;
use nom::lib::std::fmt;
use nom::lib::std::fmt::Debug;

use super::*;
use std::fmt::Display;

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    String(String),
    List(Vec<RispExp>),
    Func(RispFunc),
    Lambda(RispLambda),
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExp::Bool(a) => a.to_string(),
            RispExp::Symbol(s) => s.clone(),
            RispExp::String(s) => format!("\"{}\"", s),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            RispExp::Func(f) => format!("{}", f),
            RispExp::Lambda(l) => format!("{}", l),
        };

        write!(f, "{}", str)
    }
}

#[derive(Clone)]
pub struct RispFunc {
    pub function: fn(&[RispExp], &mut RispEnv) -> Result<RispExp, RispErr>,
    pub is_macro: bool,
    pub name: String,
    pub module: String,
}

impl PartialEq for RispFunc {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl PartialOrd for RispFunc {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

impl Debug for RispFunc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("RispFunc")
            .field("name", &self.name)
            .field("module", &self.module)
            .field("is_macro", &self.is_macro)
            .field("function", &"rust function")
            .finish()
    }
}

impl Display for RispFunc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("Native Function {}::{}", self.module, self.name))?;
        if self.is_macro {
            f.write_str(" (macro)")?;
        }

        Ok(())
    }
}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub struct RispLambda {
    pub params_exp: Rc<RispExp>,
    pub body_exp: Rc<RispExp>,
    pub name: Option<String>,
}

impl Display for RispLambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.name.is_none() {
            f.write_str("Lambda ()")
        } else {
            f.write_fmt(format_args!("Lambda ({})", self.name.clone().unwrap()))
        }
    }
}

#[derive(Debug)]
pub enum RispErr {
    Reason(String),
}
