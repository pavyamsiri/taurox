use std::time::{SystemTime, UNIX_EPOCH};

use crate::evaluator::{LoxValue, NativeFunction, RuntimeError};

#[derive(Debug)]
pub struct NativeClock;

impl NativeFunction for NativeClock {
    fn get_name(&self) -> &'static str {
        "clock"
    }

    fn call(&self, arguments: &[LoxValue]) -> Result<LoxValue, RuntimeError> {
        let _ = arguments;
        let now = SystemTime::now();
        let duration_since_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
        Ok(LoxValue::Number(
            duration_since_epoch.as_secs() as f64
                + duration_since_epoch.subsec_nanos() as f64 * 1e-9,
        ))
    }
}
