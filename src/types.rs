use nom::lib::std::cmp::Ordering;
use nom::lib::std::fmt;
use nom::lib::std::fmt::Debug;

use super::*;

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(RispFunc),
    Lambda(RispLambda),
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExp::Bool(a) => a.to_string(),
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            RispExp::Func(_) => "Function {}".to_string(),
            RispExp::Lambda(_) => "Lambda {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

#[derive(Copy, Clone)]
pub struct RispFunc(pub fn(&[RispExp]) -> Result<RispExp, RispErr>);

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
        f.write_str("Anonymous RispFunc")
    }
}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub struct RispLambda {
    pub params_exp: Rc<RispExp>,
    pub body_exp: Rc<RispExp>,
}

#[derive(Debug)]
pub enum RispErr {
    Reason(String),
}

#[derive(Clone)]
pub struct RispEnv<'a> {
    pub data: HashMap<String, RispExp>,
    pub outer: Option<&'a RispEnv<'a>>,
}

impl<'a> RispEnv<'a> {
    pub fn get(&self, key: &str) -> Option<RispExp> {
        match self.data.get(key) {
            Some(exp) => Some(exp.clone()),
            None => match self.outer {
                Some(outer_env) => outer_env.get(key),
                None => None,
            },
        }
    }
}