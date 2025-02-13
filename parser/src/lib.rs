macro_rules! dbg_println {
    ($($rest:tt)*) => {{
        #[cfg(debug_assertions)]
        std::println!($($rest)*)
    }}
}

mod bytecode;
mod coercion;
mod compiler;
mod format_ast;
mod interpreter;
mod parser;
mod type_checker;
mod type_decl;
mod type_set;
mod type_tags;
mod value;
mod vm;

pub use self::bytecode::{
    Bytecode, DebugInfo, FnBytecode, FunctionInfo, Instruction, LineInfo, OpCode,
};
pub use self::coercion::coerce_type;
pub use self::compiler::*;
pub use self::format_ast::format_stmts;
pub use self::interpreter::{run, EvalContext, EvalError, FuncDef};
pub use self::parser::{span_source as source, ArgDecl, ReadError, Span};
pub use self::type_checker::{type_check, TypeCheckContext};
pub use self::type_decl::TypeDecl;
pub use self::value::Value;
pub use self::vm::*;
