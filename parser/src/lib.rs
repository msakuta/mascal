macro_rules! dbg_println {
    ($($rest:tt)*) => {{
        crate::DEBUG_STREAM.with_borrow_mut(|s| {
            let _ = std::writeln!(s.as_mut(), $($rest)*);
        });
    }}
}

mod bytecode;
pub mod coercion;
mod compiler;
mod eval_error;
mod format_ast;
mod func;
mod interpreter;
mod iter_types;
mod parser;
pub mod type_decl;
mod type_infer;
mod type_set;
mod type_tags;
pub mod value;
mod vm;

use std::cell::RefCell;

pub use nom;

pub use self::bytecode::{
    Bytecode, DebugInfo, FnBytecode, FunctionInfo, Instruction, LineInfo, OpCode,
};
pub use self::coercion::coerce_type;
pub use self::compiler::*;
pub use self::eval_error::EvalError;
pub use self::format_ast::format_stmts;
pub use self::func::{NativeFn, NativeFnRef, UserData};
pub use self::interpreter::{EvalContext, FuncDef, NativeCode, RunResult, run};
pub use self::iter_types::{TypeParams, iter_types};
pub use self::parser::{ArgDecl, ReadError, Span, extract_structs, span_source as source};
pub use self::type_decl::TypeDecl;
pub use self::type_infer::{TypeCheckContext, TypeCheckError, type_check};
pub use self::value::Value;
pub use self::vm::*;

thread_local! {
    pub static DEBUG_STREAM: RefCell<Box<dyn std::io::Write>> = RefCell::new({
        #[cfg(debug_assertions)]
        {
            Box::new(std::io::stdout())
        }
        #[cfg(not(debug_assertions))]
        {
            Box::new(std::io::sink())
        }
    });
}
