//! LValue-related types and functions in the compiler. See also: interpreter/lvalue.rs which is defined differently.

use std::rc::Rc;

use crate::{
    compiler::size_of_struct,
    parser::{ExprEnum, Expression, StructDecl},
    OpCode,
};

use super::{
    emit_expr, error::CompileErrorKind as CEK, scalar, ty_size_of, CompileError, CompileResult,
    Compiler, Target,
};

/// Internal to the compiler
#[derive(Debug)]
pub(super) enum LValue<'src> {
    /// A variable identified by a name
    Variable(String),
    /// Reference to an element of a variable in local stack, either an array element or a struct field.
    /// Note that tuples will not use it since tuples are immutable.
    ArrayRef(usize, usize),
    /// Reference to a field of a struct, identified by a stack index and an offset.
    StructRef(Rc<StructDecl<'src>>, usize, usize),
}

pub(super) fn emit_lvalue<'src, 'env, 'c>(
    ex: &Expression<'src>,
    compiler: &'c mut Compiler<'env, 'src>,
) -> CompileResult<'src, LValue<'src>> {
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
                    scalar(ex.span, idx)?,
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

                    Ok(LValue::ArrayRef(subidx_copy, scalar(ex.span, idx)?))
                }
                LValue::StructRef(st_ty, str, subidx) => todo!(),
            }
        }
        ExprEnum::FieldAccess {
            prefix,
            postfix,
            def,
        } => {
            let span = prefix.span;
            let st_ty = def
                .as_ref()
                .ok_or_else(|| CompileError::new(ex.span, CEK::TypeNameNotFound("???".into())))?;
            let field_info = st_ty
                .field_info(**postfix, &compiler.env.typedefs)
                .ok_or_else(|| {
                    CompileError::new(*postfix, CEK::FieldNotFound(postfix.to_string()))
                })?;
            let prefix = emit_lvalue(prefix, compiler)?;
            // let stk_field_idx = compiler.find_or_create_literal(&Value::I64(field_idx as i64));
            match prefix {
                LValue::Variable(name) => Ok(LValue::StructRef(
                    st_ty.clone(),
                    compiler.find_local(&name, ex.span)?.stack_idx + field_info.offset,
                    size_of_struct(span, st_ty, &compiler.env.typedefs)?,
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

                    todo!()
                    // Ok(LValue::StructRef(subidx_copy, stk_field_idx))
                }
                LValue::StructRef(_, str, _) => {
                    let field_size =
                        ty_size_of(span, &field_info.field.ty, &compiler.env.typedefs)?;
                    Ok(LValue::StructRef(
                        st_ty.clone(),
                        str + field_info.offset,
                        field_size,
                    ))
                }
            }
        }
        _ => Err(CompileError::new(
            ex.span,
            CEK::NonLValue(ex.span.to_string()),
        )),
    }
}
