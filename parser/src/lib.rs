macro_rules! dbg_println {
    ($($rest:tt)*) => {{
        crate::DEBUG_STREAM.with_borrow_mut(|s| {
            let _ = std::writeln!(s.as_mut(), $($rest)*);
        });
    }}
}

mod bytecode;
mod coercion;
mod compiler;
mod format_ast;
mod interpreter;
mod iter_types;
mod parser;
mod type_decl;
mod type_infer;
mod type_set;
mod type_tags;
mod value;
mod vm;

use std::cell::RefCell;

pub use nom;

pub use self::bytecode::{
    Bytecode, DebugInfo, FnBytecode, FunctionInfo, Instruction, LineInfo, OpCode,
};
pub use self::coercion::coerce_type;
pub use self::compiler::*;
pub use self::format_ast::format_stmts;
pub use self::interpreter::{run, EvalContext, EvalError, FuncDef};
pub use self::iter_types::{iter_types, TypeParams};
pub use self::parser::{span_source as source, ArgDecl, ReadError, Span};
pub use self::type_decl::TypeDecl;
pub use self::type_infer::{type_check, TypeCheckContext};
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
