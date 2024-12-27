use super::SharedEnvironment;
use crate::value::{error::RuntimeError, LoxValue, NativeFunction};
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug)]
pub struct NativeClock;

impl NativeFunction for NativeClock {
    fn get_name(&self) -> &'static str {
        "clock"
    }

    fn call(&self, environment: &mut SharedEnvironment) -> Result<LoxValue, RuntimeError> {
        let _ = environment;
        let now = SystemTime::now();
        let duration_since_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
        Ok(LoxValue::Number(duration_since_epoch.as_secs() as f64))
    }

    fn get_parameters(&self) -> &'static [&'static str] {
        &[]
    }
}
