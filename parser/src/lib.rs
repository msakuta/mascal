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
mod iter_types;
mod parser;
mod type_decl;
mod type_infer;
mod type_set;
mod type_tags;
mod value;
mod vm;

pub use nom;

pub use self::bytecode::{
    Bytecode, DebugInfo, FnBytecode, FunctionInfo, Instruction, LineInfo, OpCode,
};
pub use self::coercion::coerce_type;
pub use self::compiler::*;
pub use self::format_ast::format_stmts;
pub use self::interpreter::{run, EvalContext, EvalError, FuncDef};
pub use self::iter_types::iter_types;
pub use self::parser::{span_source as source, ArgDecl, ReadError, Span};
pub use self::type_decl::TypeDecl;
pub use self::type_infer::{type_check, TypeCheckContext};
pub use self::value::Value;
pub use self::vm::*;
