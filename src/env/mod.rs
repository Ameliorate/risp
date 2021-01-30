use crate::env::cmp::CmpModule;
use crate::env::lambda::LambdaModule;
use crate::env::logic::LogicModule;
use crate::env::math::MathModule;
use crate::{parse, RispErr, RispExp, RispFunc};
use std::collections::HashMap;
use std::rc::Rc;
use crate::env::def::DefModule;

pub mod cmp;
pub mod def;
pub mod lambda;
pub mod logic;
pub mod math;

#[derive(Clone)]
pub struct RispEnv<'a> {
    pub data: HashMap<String, RispExp>,
    pub outer: Option<&'a RispEnv<'a>>,
}

impl<'a> RispEnv<'a> {
    pub fn empty_env() -> RispEnv<'a> {
        let data: HashMap<String, RispExp> = HashMap::new();

        RispEnv { data, outer: None }
    }

    pub fn default_env() -> RispEnv<'a> {
        let mut env = RispEnv::empty_env();

        env.add_native_module(MathModule);
        env.add_native_module(CmpModule);
        env.add_native_module(LogicModule);
        env.add_native_module(LambdaModule);
        env.add_native_module(DefModule);

        env
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

    pub fn add(&mut self, key: &str, val: RispExp) {
        if self.get(key).is_some() {
            panic!(format!(
                "Add called on env with key {} but already has a value",
                key
            ))
        }

        self.set(key, val)
    }

    pub fn set(&mut self, key: &str, val: RispExp) {
        self.data.insert(key.to_string(), val);
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

    pub fn add_native_module(&mut self, module: impl NativeModule) {
        module.load(self);
    }
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

pub trait NativeModule {
    fn load(&self, env: &mut RispEnv);
    fn name() -> &'static str;

    fn add_native_fn(
        env: &mut RispEnv,
        name: &str,
        is_macro: bool,
        function: fn(&[RispExp], &mut RispEnv) -> Result<RispExp, RispErr>,
    ) {
        env.add(
            name,
            RispExp::Func(RispFunc {
                function,
                name: name.to_string(),
                module: Self::name().to_string(),
                is_macro,
            }),
        )
    }
}
