use crate::{RispErr, RispExp, RispFunc, RispLambda, parse};
use std::collections::HashMap;
use std::rc::Rc;

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        RispFunc {
            function: |args: &[RispExp], _: &mut RispEnv| -> Result<RispExp, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = floats
                    .first()
                    .ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                let rest = &floats[1..];
                fn f(prev: &f64, xs: &[f64]) -> bool {
                    match xs.first() {
                        Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                        None => true,
                    }
                };
                Ok(RispExp::Bool(f(first, rest)))
            },
            is_macro: false,
        }
    }};
}

#[derive(Clone)]
pub struct RispEnv<'a> {
    pub data: HashMap<String, RispExp>,
    pub outer: Option<&'a RispEnv<'a>>,
}

impl<'a> RispEnv<'a> {
    pub fn default_env() -> RispEnv<'a> {
        let mut data: HashMap<String, RispExp> = HashMap::new();
        data.insert(
            "+".to_string(),
            RispExp::Func(RispFunc {
                function: |args: &[RispExp], _: &mut RispEnv| -> Result<RispExp, RispErr> {
                    let sum = parse_list_of_floats(args)?
                        .iter()
                        .fold(0.0, |sum, a| sum + a);

                    Ok(RispExp::Number(sum))
                },
                is_macro: false,
            }),
        );
        data.insert(
            "-".to_string(),
            RispExp::Func(RispFunc {
                function: |args: &[RispExp], _: &mut RispEnv| -> Result<RispExp, RispErr> {
                    let floats = parse_list_of_floats(args)?;
                    let first = *floats
                        .first()
                        .ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                    let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                    Ok(RispExp::Number(first - sum_of_rest))
                },
                is_macro: false,
            }),
        );
        data.insert(
            "=".to_string(),
            RispExp::Func(ensure_tonicity!(|a, b| a == b)),
        );
        data.insert(
            ">".to_string(),
            RispExp::Func(ensure_tonicity!(|a, b| a > b)),
        );
        data.insert(
            ">=".to_string(),
            RispExp::Func(ensure_tonicity!(|a, b| a >= b)),
        );
        data.insert(
            "<".to_string(),
            RispExp::Func(ensure_tonicity!(|a, b| a < b)),
        );
        data.insert(
            "<=".to_string(),
            RispExp::Func(ensure_tonicity!(|a, b| a <= b)),
        );
        data.insert(
            "if".to_string(),
            RispExp::Func(RispFunc {
                function: if_function,
                is_macro: false,
            }),
        );
        data.insert(
            "def".to_string(),
            RispExp::Func(RispFunc {
                function: def_function,
                is_macro: true,
            }),
        );
        data.insert(
            "fn".to_string(),
            RispExp::Func(RispFunc {
                function: lambda_function,
                is_macro: true,
            }),
        );

        RispEnv { data, outer: None }
    }

    pub fn get(&self, key: &str) -> Option<RispExp> {
        match self.data.get(key) {
            Some(exp) => Some(exp.clone()),
            None => match self.outer {
                Some(outer_env) => outer_env.get(key),
                None => None,
            },
        }
    }

    pub fn eval(&mut self, exp: &RispExp) -> Result<RispExp, RispErr> {
        match exp {
            RispExp::Symbol(k) => self
                .get(k)
                .ok_or(RispErr::Reason(format!("unexpected symbol k='{}'", k))),
            RispExp::Bool(_a) => Ok(exp.clone()),
            RispExp::Number(_a) => Ok(exp.clone()),
            RispExp::String(_a) => Ok(exp.clone()),

            RispExp::List(list) => {
                let first_form = list
                    .first()
                    .ok_or(RispErr::Reason("expected a non-empty list".to_string()))?;
                let arg_forms = &list[1..];
                let first_eval = self.eval(first_form)?;
                match first_eval {
                    RispExp::Func(f) => {
                        if f.is_macro {
                            (f.function)(arg_forms, self)
                        } else {
                            (f.function)(&self.eval_forms(arg_forms)?, self)
                        }
                    }
                    RispExp::Lambda(lambda) => {
                        let mut new_env = env_for_lambda(lambda.params_exp, arg_forms, self)?;
                        new_env.eval(&lambda.body_exp)
                    }
                    _ => Err(RispErr::Reason("first form must be a function".to_string())),
                }
            }
            RispExp::Func(_) => Err(RispErr::Reason("unexpected form".to_string())),
            RispExp::Lambda(_) => Err(RispErr::Reason("unexpected form".to_string())),
        }
    }

    pub fn parse_eval(&mut self, expr: String) -> Result<RispExp, RispErr> {
        let parsed_exp = parse(&expr)?;

        if parsed_exp.is_empty() {
            return Ok(RispExp::List(Vec::new()));
        }

        let mut evaled_exp: Option<RispExp> = None;

        for code in parsed_exp {
            evaled_exp = Some(self.eval(&code)?);
        }

        Ok(evaled_exp.unwrap())
    }

    /// Replaces symbols with their values as variables, and evaluates nested lists
    fn eval_forms(&mut self, arg_forms: &[RispExp]) -> Result<Vec<RispExp>, RispErr> {
        arg_forms.iter().map(|x| self.eval(x)).collect()
    }
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
    match exp {
        RispExp::Number(num) => Ok(*num),
        _ => Err(RispErr::Reason("expected a number".to_string())),
    }
}

fn if_function(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let test_form = arg_forms
        .first()
        .ok_or(RispErr::Reason("expected test form".to_string()))?;
    let test_eval = env.eval(test_form)?;
    match test_eval {
        RispExp::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let res_form = arg_forms
                .get(form_idx)
                .ok_or(RispErr::Reason(format!("expected form idx={}", form_idx)))?;
            let res_eval = env.eval(res_form);

            res_eval
        }
        _ => Err(RispErr::Reason(format!(
            "unexpected test form='{}'",
            test_form.to_string()
        ))),
    }
}

fn def_function(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let first_form = arg_forms
        .first()
        .ok_or(RispErr::Reason("expected first form".to_string()))?;
    let first_str = match first_form {
        RispExp::Symbol(s) => Ok(s.clone()),
        _ => Err(RispErr::Reason(
            "expected first form to be a symbol".to_string(),
        )),
    }?;
    let second_form = arg_forms
        .get(1)
        .ok_or(RispErr::Reason("expected second form".to_string()))?;
    if arg_forms.len() > 2 {
        return Err(RispErr::Reason("def can only have two forms ".to_string()));
    }
    let second_eval = env.eval(second_form)?;
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn lambda_function(arg_forms: &[RispExp], _: &mut RispEnv) -> Result<RispExp, RispErr> {
    let params_exp = arg_forms
        .first()
        .ok_or(RispErr::Reason("expected args form".to_string()))?;
    let body_exp = arg_forms
        .get(1)
        .ok_or(RispErr::Reason("expected second form".to_string()))?;
    if arg_forms.len() > 2 {
        return Err(RispErr::Reason(
            "fn definition can only have two forms ".to_string(),
        ));
    }

    Ok(RispExp::Lambda(RispLambda {
        body_exp: Rc::new(body_exp.clone()),
        params_exp: Rc::new(params_exp.clone()),
    }))
}

fn parse_list_of_symbol_strings(form: Rc<RispExp>) -> Result<Vec<String>, RispErr> {
    let list = match form.as_ref() {
        RispExp::List(s) => Ok(s.clone()),
        _ => Err(RispErr::Reason(
            "expected args form to be a list".to_string(),
        )),
    }?;
    list.iter()
        .map(|x| match x {
            RispExp::Symbol(s) => Ok(s.clone()),
            _ => Err(RispErr::Reason(
                "expected symbols in the argument list".to_string(),
            )),
        })
        .collect()
}

fn env_for_lambda<'a>(
    params: Rc<RispExp>,
    arg_forms: &[RispExp],
    outer_env: &'a mut RispEnv,
) -> Result<RispEnv<'a>, RispErr> {
    let ks = parse_list_of_symbol_strings(params)?;
    if ks.len() != arg_forms.len() {
        return Err(RispErr::Reason(format!(
            "expected {} arguments, got {}",
            ks.len(),
            arg_forms.len()
        )));
    }
    let vs = outer_env.eval_forms(arg_forms)?;
    let mut data: HashMap<String, RispExp> = HashMap::new();
    for (k, v) in ks.iter().zip(vs.iter()) {
        data.insert(k.clone(), v.clone());
    }
    Ok(RispEnv {
        data,
        outer: Some(outer_env),
    })
}
