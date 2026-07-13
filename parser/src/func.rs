//! Function types
//!
//! The shared function pointer types among the interpreter and the Vm.

use std::rc::Rc;

use crate::{EvalError, Value};

pub type UserData = Rc<dyn std::any::Any>;

pub type NativeFnRef = dyn Fn(&UserData, &[Value]) -> Result<Value, EvalError>;
pub type NativeFn = Box<NativeFnRef>;
