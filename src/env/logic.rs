use crate::{NativeModule, RispEnv, RispErr, RispExp};

pub struct LogicModule;

impl NativeModule for LogicModule {
    fn load(&self, env: &mut RispEnv) {
        Self::add_native_fn(env, "if", true, if_function);
    }

    fn name() -> &'static str {
        "logic"
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
