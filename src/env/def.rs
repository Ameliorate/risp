use crate::{NativeModule, RispEnv, RispErr, RispExp, RispLambda};

pub struct DefModule;

impl NativeModule for DefModule {
    fn load(&self, env: &mut RispEnv) {
        Self::add_native_fn(env, "def", true, def_function);
    }

    fn name() -> &'static str {
        "def"
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
    let mut second_eval = env.eval(second_form)?;
    match second_eval {
        RispExp::Lambda(lambda) => {
            second_eval = RispExp::Lambda(RispLambda {
                params_exp: lambda.params_exp,
                body_exp: lambda.body_exp,
                name: Some(first_str.clone())
            })
        },
        _ => (),
    };
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}
