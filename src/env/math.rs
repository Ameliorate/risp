use crate::{NativeModule, RispEnv, RispErr, RispExp};

pub struct MathModule;

impl NativeModule for MathModule {
    fn load(&self, env: &mut RispEnv) {
        Self::add_native_fn(
            env,
            "+",
            false,
            |args: &[RispExp], _: &mut RispEnv| -> Result<RispExp, RispErr> {
                let sum = parse_list_of_floats(args)?
                    .iter()
                    .fold(0.0, |sum, a| sum + a);

                Ok(RispExp::Number(sum))
            },
        );

        Self::add_native_fn(
            env,
            "-",
            false,
            |args: &[RispExp], _: &mut RispEnv| -> Result<RispExp, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats
                    .first()
                    .ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(RispExp::Number(first - sum_of_rest))
            },
        );
    }

    fn name() -> &'static str {
        "math"
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
