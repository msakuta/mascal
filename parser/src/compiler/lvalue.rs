//! LValue-related types and functions in the compiler. See also: interpreter/lvalue.rs which is defined differently.

use crate::{
    parser::{ExprEnum, Expression},
    OpCode,
};

use super::{
    emit_expr, error::CompileErrorKind as CEK, CompileError, CompileResult, Compiler, Target,
};

/// Internal to the compiler
pub(super) enum LValue {
    /// A variable identified by a name
    Variable(String),
    /// Reference to an array element of a variable in local stack, e.g. an array element.
    ArrayRef(usize, usize),
}

pub(super) fn emit_lvalue<'src, 'env, 'c>(
    ex: &Expression<'src>,
    compiler: &'c mut Compiler<'env>,
) -> CompileResult<'src, LValue> {
    match &ex.expr {
        ExprEnum::NumLiteral(_, _)
        | ExprEnum::StrLiteral(_)
        | ExprEnum::ArrLiteral(_)
        | ExprEnum::TupleLiteral(_) => Err(CompileError::new(
            ex.span,
            CEK::AssignToLiteral(ex.span.to_string()),
        )),
        ExprEnum::Variable(name) => Ok(LValue::Variable(name.to_string())),
        ExprEnum::Cast(_, _) | ExprEnum::FnInvoke(_, _) => Err(CompileError::new(
            ex.span,
            CEK::NonLValue(ex.span.to_string()),
        )),
        ExprEnum::VarAssign(ex, _) => emit_lvalue(ex, compiler),
        ExprEnum::ArrIndex(ex, idx) => {
            let idx = emit_expr(&idx[0], compiler)?;
            let arr = emit_lvalue(ex, compiler)?;
            match arr {
                LValue::Variable(name) => Ok(LValue::ArrayRef(
                    compiler.find_local(&name, ex.span)?.stack_idx,
                    idx,
                )),
                LValue::ArrayRef(arr, subidx) => {
                    let subidx_copy = compiler.target_stack.len();
                    compiler.target_stack.push(Target::None);

                    // First, copy the index to be overwritten by Get instruction
                    compiler
                        .bytecode
                        .push_inst(OpCode::Move, subidx as u8, subidx_copy as u16);

                    // Second, get the element from the array reference
                    compiler
                        .bytecode
                        .push_inst(OpCode::Get, arr as u8, subidx_copy as u16);

                    Ok(LValue::ArrayRef(subidx_copy, idx))
                }
            }
        }
        _ => Err(CompileError::new(
            ex.span,
            CEK::NonLValue(ex.span.to_string()),
        )),
    }
}
