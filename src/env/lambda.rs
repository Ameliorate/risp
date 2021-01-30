use crate::{NativeModule, RispEnv, RispErr, RispExp, RispLambda};
use std::rc::Rc;

pub struct LambdaModule;

impl NativeModule for LambdaModule {
    fn load(&self, env: &mut RispEnv) {
        Self::add_native_fn(env, "\\", true, lambda_function);
    }

    fn name() -> &'static str {
        "lambda"
    }
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
        name: None,
    }))
}
