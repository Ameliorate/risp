use crate::{NativeModule, RispEnv, RispErr, RispExp};

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[RispExp], _: &mut RispEnv| -> Result<RispExp, RispErr> {
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
        }
    }};
}

pub struct CmpModule;

impl NativeModule for CmpModule {
    fn load(&self, env: &mut RispEnv) {
        Self::add_native_fn(env, "=", false, ensure_tonicity!(|a, b| a == b));
        Self::add_native_fn(env, ">", false, ensure_tonicity!(|a, b| a > b));
        Self::add_native_fn(env, ">=", false, ensure_tonicity!(|a, b| a >= b));
        Self::add_native_fn(env, "<", false, ensure_tonicity!(|a, b| a < b));
        Self::add_native_fn(env, "<=", false, ensure_tonicity!(|a, b| a <= b));
    }

    fn name() -> &'static str {
        "cmp"
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
