use std::{collections::HashMap, fmt::Display};

use crate::{
    interpreter::{std_functions, FuncCode},
    parser::{ExprEnum, Expression, Statement},
    type_decl::{ArraySize, TypeDecl},
    FuncDef, Span, Value,
};

#[derive(Debug, PartialEq)]
pub struct TypeCheckError<'src> {
    msg: String,
    span: Span<'src>,
    source_file: Option<&'src str>,
}

impl<'src> Display for TypeCheckError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}:{}:{}",
            self.msg,
            self.source_file.unwrap_or("<unknown>"),
            self.span.location_line(),
            self.span.get_utf8_column()
        )
    }
}

impl<'src> TypeCheckError<'src> {
    fn new(msg: String, span: Span<'src>, source_file: Option<&'src str>) -> Self {
        Self {
            msg,
            span,
            source_file,
        }
    }

    pub(crate) fn undefined_fn(
        name: &str,
        span: Span<'src>,
        source_file: Option<&'src str>,
    ) -> Self {
        Self::new(
            format!("function {} is not defined", name),
            span,
            source_file,
        )
    }

    pub(crate) fn undefined_arg(
        name: &str,
        span: Span<'src>,
        source_file: Option<&'src str>,
    ) -> Self {
        Self::new(
            format!("argument {} is not defined", name),
            span,
            source_file,
        )
    }

    pub(crate) fn undefined_var(
        name: &str,
        span: Span<'src>,
        source_file: Option<&'src str>,
    ) -> Self {
        Self::new(
            format!("variable {} is not defined", name),
            span,
            source_file,
        )
    }

    pub(crate) fn unassigned_arg(
        name: &str,
        span: Span<'src>,
        source_file: Option<&'src str>,
    ) -> Self {
        Self::new(
            format!("argument {} is not assigned in function invocation", name),
            span,
            source_file,
        )
    }
}

#[derive(Clone)]
pub struct TypeCheckContext<'src, 'native, 'ctx> {
    /// Variables table for type checking.
    variables: HashMap<&'src str, TypeDecl>,
    /// Function names are owned strings because it can be either from source or native.
    functions: HashMap<String, FuncDef<'src, 'native>>,
    super_context: Option<&'ctx TypeCheckContext<'src, 'native, 'ctx>>,
    source_file: Option<&'src str>,
}

impl<'src, 'native, 'ctx> TypeCheckContext<'src, 'native, 'ctx> {
    pub fn new(source_file: Option<&'src str>) -> Self {
        Self {
            variables: HashMap::new(),
            functions: std_functions(),
            super_context: None,
            source_file,
        }
    }

    fn get_var(&self, name: &str) -> Option<TypeDecl> {
        if let Some(val) = self.variables.get(name) {
            Some(val.clone())
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_var(name)
        } else {
            None
        }
    }

    pub fn set_fn(&mut self, name: &str, fun: FuncDef<'src, 'native>) {
        self.functions.insert(name.to_string(), fun);
    }

    fn get_fn(&self, name: &str) -> Option<&FuncDef<'src, 'native>> {
        if let Some(val) = self.functions.get(name) {
            Some(val)
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_fn(name)
        } else {
            None
        }
    }

    fn push_stack(super_ctx: &'ctx Self) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            super_context: Some(super_ctx),
            source_file: super_ctx.source_file,
        }
    }
}

fn tc_expr<'src, 'b, 'native>(
    e: &'b Expression<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>>
where
    'native: 'src,
{
    Ok(match &e.expr {
        ExprEnum::NumLiteral(val) => match val {
            Value::F64(_) | Value::F32(_) => TypeDecl::Float,
            Value::I64(_) | Value::I32(_) => TypeDecl::Integer,
            _ => {
                return Err(TypeCheckError::new(
                    "Numeric literal has a non-number value".to_string(),
                    e.span,
                    ctx.source_file,
                ))
            }
        },
        ExprEnum::StrLiteral(_val) => TypeDecl::Str,
        ExprEnum::ArrLiteral(val) => {
            if !val.is_empty() {
                for (ex1, ex2) in val[..val.len() - 1].iter().zip(val[1..].iter()) {
                    let el1 = tc_expr(ex1, ctx)?;
                    let el2 = tc_expr(ex2, ctx)?;
                    if el1 != el2 {
                        return Err(TypeCheckError::new(
                            format!("Types in an array is not homogeneous: {el1:?} and {el2:?}"),
                            e.span,
                            ctx.source_file,
                        ));
                    }
                }
            }
            let ty = val
                .first()
                .map(|e| tc_expr(e, ctx))
                .unwrap_or(Ok(TypeDecl::Any))?;
            TypeDecl::Array(Box::new(ty), ArraySize::Fixed(val.len()))
        }
        ExprEnum::TupleLiteral(val) => {
            let ty = val
                .iter()
                .map(|e| tc_expr(e, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            TypeDecl::Tuple(ty)
        }
        ExprEnum::Variable(str) => ctx
            .get_var(str)
            .ok_or_else(|| TypeCheckError::undefined_var(str, e.span, ctx.source_file))?,
        ExprEnum::Cast(ex, decl) => {
            let res = tc_expr(ex, ctx)?;
            tc_cast_type(&res, decl, ex.span, ctx)?
        }
        ExprEnum::VarAssign(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Assignment")?,
        ExprEnum::FnInvoke(fname, args) => {
            let fn_args = ctx
                .get_fn(*fname)
                .ok_or_else(|| TypeCheckError::undefined_fn(*fname, e.span, ctx.source_file))?
                .args()
                .clone();

            let mut ty_args = vec![None; fn_args.len().max(args.len())];

            // Fill unnamed args
            for (arg, ty_arg) in args.iter().zip(ty_args.iter_mut()) {
                if arg.name.is_none() {
                    *ty_arg = Some(tc_expr(&arg.expr, ctx)?);
                }
            }

            // Find and assign named args
            for arg in args.iter() {
                if let Some(name) = arg.name {
                    if let Some(ty_arg) = fn_args
                        .iter()
                        .enumerate()
                        .find(|f| f.1.name == *name)
                        .and_then(|(i, _)| ty_args.get_mut(i))
                    {
                        *ty_arg = Some(tc_expr(&arg.expr, ctx)?);
                    } else {
                        return Err(TypeCheckError::undefined_arg(
                            *name,
                            e.span,
                            ctx.source_file,
                        ));
                    }
                }
            }

            for (ty_arg, decl) in ty_args.iter().zip(fn_args.iter()) {
                let Some(ty_arg) = ty_arg.as_ref().or_else(|| {
                    if decl.init.is_some() {
                        Some(&decl.ty)
                    } else {
                        None
                    }
                }) else {
                    return Err(TypeCheckError::unassigned_arg(
                        decl.name,
                        e.span,
                        ctx.source_file,
                    ));
                };
                tc_coerce_type(&ty_arg, &decl.ty, e.span, ctx)?;
            }

            let func = ctx
                .get_fn(*fname)
                .ok_or_else(|| TypeCheckError::undefined_fn(fname, e.span, ctx.source_file))?;
            match func {
                FuncDef::Code(code) => code.ret_type.clone().unwrap_or(TypeDecl::Any),
                FuncDef::Native(native) => native.ret_type.clone().unwrap_or(TypeDecl::Any),
            }
        }
        ExprEnum::ArrIndex(ex, args) => {
            let arg_types = args
                .iter()
                .map(|v| tc_expr(v, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let _arg0 = {
                let v = arg_types[0].clone();
                if let TypeDecl::I64 = tc_coerce_type(&v, &TypeDecl::I64, args[0].span, ctx)? {
                    v
                } else {
                    return Err(TypeCheckError::new(
                        "Subscript type should be integer types".to_string(),
                        args.first().unwrap_or(e).span,
                        ctx.source_file,
                    ));
                }
            };
            if let TypeDecl::Array(inner, _) = tc_expr(ex, ctx)? {
                *inner.clone()
            } else {
                return Err(TypeCheckError::new(
                    "Subscript operator's first operand is not an array".to_string(),
                    ex.span,
                    ctx.source_file,
                ));
            }
        }
        ExprEnum::TupleIndex(ex, index) => {
            let result = tc_expr(ex, ctx)?;
            if let TypeDecl::Tuple(inner) = result {
                inner
                    .get(*index)
                    .ok_or_else(|| {
                        TypeCheckError::new(
                            "Tuple index out of range".to_string(),
                            ex.span,
                            ctx.source_file,
                        )
                    })?
                    .clone()
            } else {
                return Err(TypeCheckError::new(
                    "Tuple index applied to a non-tuple".to_string(),
                    ex.span,
                    ctx.source_file,
                ));
            }
        }
        ExprEnum::Not(val) => {
            tc_expr(val, ctx)?;
            // The result of logical operator should be i32 (bool)
            TypeDecl::I32
        }
        ExprEnum::BitNot(val) => tc_expr(val, ctx)?,
        ExprEnum::Neg(val) => tc_expr(val, ctx)?,
        ExprEnum::Add(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Add")?,
        ExprEnum::Sub(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Sub")?,
        ExprEnum::Mult(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Mult")?,
        ExprEnum::Div(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Div")?,
        ExprEnum::LT(lhs, rhs) => binary_cmp(&lhs, &rhs, e.span, ctx, "LT")?,
        ExprEnum::GT(lhs, rhs) => binary_cmp(&lhs, &rhs, e.span, ctx, "GT")?,
        ExprEnum::BitAnd(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "BitAnd")?,
        ExprEnum::BitXor(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "BitXor")?,
        ExprEnum::BitOr(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "BitOr")?,
        ExprEnum::And(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "And")?,
        ExprEnum::Or(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Or")?,
        ExprEnum::Conditional(cond, true_branch, false_branch) => {
            tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::I32, cond.span, ctx)?;
            let true_type = type_check(true_branch, ctx)?;
            if let Some(false_type) = false_branch {
                let false_type = type_check(false_type, ctx)?;
                binary_op_type(&true_type, &false_type, e.span, ctx).map_err(|_| {
                    TypeCheckError::new(
                        format!("Conditional expression doesn't have the compatible types in true and false branch: {:?} and {:?}", true_type, false_type),
                        e.span,
                        ctx.source_file
                    )
                })?
            } else {
                true_type
            }
        }
        ExprEnum::Brace(stmts) => {
            let mut subctx = TypeCheckContext::push_stack(ctx);
            type_check(stmts, &mut subctx)?
        }
    })
}

fn tc_array_size(value: &ArraySize, target: &ArraySize) -> Result<(), String> {
    match (value, target) {
        (_, ArraySize::Any) => {}
        (ArraySize::Fixed(v_len), ArraySize::Fixed(t_len)) => {
            if v_len != t_len {
                return Err(format!(
                    "Array size is not compatible: {v_len} cannot assign to {t_len}"
                ));
            }
        }
        (ArraySize::Range(v_range), ArraySize::Range(t_range)) => {
            array_range_verify(v_range)?;
            array_range_verify(t_range)?;
            if t_range.end < v_range.end || v_range.start < t_range.start {
                return Err(format!(
                    "Array range is not compatible: {value} cannot assign to {target}"
                ));
            }
        }
        (ArraySize::Fixed(v_len), ArraySize::Range(t_range)) => {
            array_range_verify(t_range)?;
            if *v_len < t_range.start || t_range.end < *v_len {
                return Err(format!(
                    "Array range is not compatible: {v_len} cannot assign to {target}"
                ));
            }
        }
        (ArraySize::Any, ArraySize::Range(t_range)) => {
            array_range_verify(t_range)?;
        }
        _ => {
            return Err(format!(
                "Array size constraint is not compatible between {value:?} and {target:?}"
            ));
        }
    }
    Ok(())
}

fn tc_coerce_type<'src>(
    value: &TypeDecl,
    target: &TypeDecl,
    span: Span<'src>,
    ctx: &TypeCheckContext<'src, '_, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    Ok(match (value, target) {
        (_, Any) => value.clone(),
        (Any, _) => target.clone(),
        (F64 | Float, F64) => F64,
        (F32 | Float, F32) => F32,
        (I64 | Integer, I64) => I64,
        (I32 | Integer, I32) => I32,
        (Str, Str) => Str,
        (Array(v_inner, v_len), Array(t_inner, t_len)) => {
            tc_array_size(v_len, t_len)
                .map_err(|e| TypeCheckError::new(e, span, ctx.source_file))?;
            Array(
                Box::new(tc_coerce_type(v_inner, t_inner, span, ctx)?),
                t_len.clone(),
            )
        }
        (Float, Float) => Float,
        (Integer, Integer) => Integer,
        (Tuple(v_inner), Tuple(t_inner)) => {
            if v_inner.len() != t_inner.len() {
                return Err(TypeCheckError::new(
                    "Tuples size does not match".to_string(),
                    span,
                    ctx.source_file,
                ));
            }
            Tuple(
                v_inner
                    .iter()
                    .zip(t_inner.iter())
                    .map(|(v, t)| tc_coerce_type(v, t, span, ctx))
                    .collect::<Result<_, _>>()?,
            )
        }
        _ => {
            return Err(TypeCheckError::new(
                format!(
                    "Type check error! {:?} cannot be assigned to {:?}",
                    value, target
                ),
                span,
                ctx.source_file,
            ))
        }
    })
}

fn array_range_verify(range: &std::ops::Range<usize>) -> Result<(), String> {
    if range.end < range.start {
        return Err(format!(
            "Array size has invalid range: {range:?}; start should be less than end"
        ));
    }
    Ok(())
}

fn tc_cast_type<'src>(
    value: &TypeDecl,
    target: &TypeDecl,
    span: Span<'src>,
    ctx: &TypeCheckContext<'src, '_, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    Ok(match (value, target) {
        (_, Any) => value.clone(),
        (Any, _) => target.clone(),
        (I32 | I64 | F32 | F64 | Integer | Float, F64) => F64,
        (I32 | I64 | F32 | F64 | Integer | Float, F32) => F32,
        (I32 | I64 | F32 | F64 | Integer | Float, I64) => I64,
        (I32 | I64 | F32 | F64 | Integer | Float, I32) => I32,
        (Str, Str) => Str,
        (Array(v_inner, v_len), Array(t_inner, t_len)) => {
            if let Some((v_len, t_len)) = v_len.zip(t_len) {
                if v_len < t_len {
                    return Err(TypeCheckError::new(
                        "Assignee array is smaller than assigner".to_string(),
                        span,
                        ctx.source_file,
                    ));
                }
            }
            // Array doesn't recursively type cast for performance reasons.
            Array(
                Box::new(tc_coerce_type(v_inner, t_inner, span, ctx)?),
                t_len.clone(),
            )
        }
        (I32 | I64 | F32 | F64 | Integer | Float, Float) => Float,
        (I32 | I64 | F32 | F64 | Integer | Float, Integer) => Integer,
        _ => {
            return Err(TypeCheckError::new(
                format!(
                    "Type check error! {:?} cannot be casted to {:?}",
                    value, target
                ),
                span,
                ctx.source_file,
            ))
        }
    })
}

pub fn type_check<'src, 'ast, 'native>(
    stmts: &'ast Vec<Statement<'src>>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>>
where
    'native: 'src,
{
    let mut res = TypeDecl::Any;
    for stmt in stmts {
        match stmt {
            Statement::VarDecl(var, type_, initializer) => {
                let init_type = if let Some(init_expr) = initializer {
                    let init_type = tc_expr(init_expr, ctx)?;
                    tc_coerce_type(&init_type, type_, init_expr.span, ctx)?
                } else {
                    type_.clone()
                };
                ctx.variables.insert(**var, init_type);
            }
            Statement::FnDecl {
                name,
                args,
                ret_type,
                stmts,
            } => {
                // Function declaration needs to be added first to allow recursive calls
                ctx.functions.insert(
                    name.to_string(),
                    FuncDef::Code(FuncCode::new(stmts.clone(), args.clone(), ret_type.clone())),
                );
                let mut subctx = TypeCheckContext::push_stack(ctx);
                for arg in args.iter() {
                    if let Some(ref init) = arg.init {
                        // Use a new context to denote constant expression
                        let init_ty = tc_expr(init, &mut TypeCheckContext::new(ctx.source_file))?;
                        tc_coerce_type(
                            &init_ty,
                            &arg.ty,
                            Span::new(name),
                            &mut TypeCheckContext::new(ctx.source_file),
                        )?;
                    }
                    subctx.variables.insert(arg.name, arg.ty.clone());
                }
                let last_stmt = type_check(stmts, &mut subctx)?;
                if let Some((ret_type, Statement::Expression(ret_expr))) =
                    ret_type.as_ref().zip(stmts.last())
                {
                    tc_coerce_type(&last_stmt, &ret_type, ret_expr.span, ctx)?;
                }
            }
            Statement::Expression(e) => {
                res = tc_expr(&e, ctx)?;
            }
            Statement::Loop(e) => {
                res = type_check(e, ctx)?;
            }
            Statement::While(cond, e) => {
                tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::I32, cond.span, ctx).map_err(
                    |e| {
                        TypeCheckError::new(
                            format!("Type error in condition: {e}"),
                            cond.span,
                            ctx.source_file,
                        )
                    },
                )?;
                res = type_check(e, ctx)?;
            }
            Statement::For(iter, from, to, e) => {
                tc_coerce_type(&tc_expr(from, ctx)?, &TypeDecl::I64, from.span, ctx)?;
                tc_coerce_type(&tc_expr(to, ctx)?, &TypeDecl::I64, to.span, ctx)?;
                ctx.variables.insert(iter, TypeDecl::I64);
                res = type_check(e, ctx)?;
            }
            Statement::Break => {
                // TODO: check types in break out site. For now we disallow break with values like Rust.
            }
            Statement::Comment(_) => (),
        }
    }
    Ok(res)
}

fn binary_op_gen<'src, 'ast, 'native>(
    lhs: &'ast Expression<'src>,
    rhs: &'ast Expression<'src>,
    span: Span<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
    op: &str,
    mut f: impl FnMut(
        &TypeDecl,
        &TypeDecl,
        Span<'src>,
        &TypeCheckContext<'src, 'native, '_>,
    ) -> Result<TypeDecl, TypeCheckError<'src>>,
) -> Result<TypeDecl, TypeCheckError<'src>>
where
    'native: 'src,
{
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;
    f(&lhst, &rhst, span, ctx).map_err(|e| {
        TypeCheckError::new(
            format!(
                "Operation {op} between incompatible type {} and {}: {}",
                lhst, rhst, e.msg
            ),
            lhs.span,
            ctx.source_file,
        )
    })
}

fn binary_op<'src, 'ast, 'native>(
    lhs: &'ast Expression<'src>,
    rhs: &'ast Expression<'src>,
    span: Span<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>>
where
    'native: 'src,
{
    binary_op_gen(lhs, rhs, span, ctx, op, binary_op_type)
}

fn binary_op_type<'src>(
    lhs: &TypeDecl,
    rhs: &TypeDecl,
    span: Span<'src>,
    ctx: &TypeCheckContext<'src, '_, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    let res = match (&lhs, &rhs) {
        // `Any` type spreads contamination in the source code.
        (Any, _) => Any,
        (_, Any) => Any,
        (F64, F64) => F64,
        (F32, F32) => F32,
        (I64, I64) => I64,
        (I32, I32) => I32,
        (Str, Str) => Str,
        (Float, Float) => Float,
        (Integer, Integer) => Integer,
        (Float, F64) | (F64, Float) => F64,
        (Float, F32) | (F32, Float) => F32,
        (Integer, I64) | (I64, Integer) => I64,
        (Integer, I32) | (I32, Integer) => I32,
        (Array(lhs, lhs_len), Array(rhs, rhs_len)) => {
            tc_array_size(rhs_len, lhs_len)
                .map_err(|e| TypeCheckError::new(e, span, ctx.source_file))?;
            if let Some((lhs_len, rhs_len)) = lhs_len.zip(rhs_len) {
                if lhs_len < rhs_len {
                    return Err(TypeCheckError::new(
                        "Binary operation between an array with different length".to_string(),
                        span,
                        ctx.source_file,
                    ));
                }
            }
            return Ok(Array(
                Box::new(binary_op_type(lhs, rhs, span, ctx)?),
                lhs_len.or(rhs_len),
            ));
        }
        _ => {
            return Err(TypeCheckError::new(
                "Binary operation incompatible".to_string(),
                span,
                ctx.source_file,
            ))
        }
    };
    Ok(res)
}

fn binary_cmp<'src, 'ast, 'native>(
    lhs: &'ast Expression<'src>,
    rhs: &'ast Expression<'src>,
    span: Span<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>>
where
    'native: 'src,
{
    binary_op_gen(lhs, rhs, span, ctx, op, binary_cmp_type)
}

/// Binary comparison operator type check. It will always return i32, which is used as a bool in this language.
fn binary_cmp_type<'src>(
    lhs: &TypeDecl,
    rhs: &TypeDecl,
    span: Span<'src>,
    ctx: &TypeCheckContext<'src, '_, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    let res = match (&lhs, &rhs) {
        (Any, _) => I32,
        (_, Any) => I32,
        (F64, F64) => I32,
        (F32, F32) => I32,
        (I64, I64) => I32,
        (I32, I32) => I32,
        (Str, Str) => I32,
        (Float, Float) => I32,
        (Integer, Integer) => I32,
        (Float, F64 | F32) | (F64 | F32, Float) => I32,
        (Integer, I64 | I32) | (I64 | I32, Integer) => I32,
        (Array(lhs, lhs_len), Array(rhs, rhs_len)) => {
            if lhs_len != rhs_len {
                return Err(TypeCheckError::new(
                    "Array size must be the same for comparison".to_string(),
                    span,
                    ctx.source_file,
                ));
            }
            return binary_cmp_type(lhs, rhs, span, ctx);
        }
        _ => {
            return Err(TypeCheckError::new(
                "Binary comparison incompatible".to_string(),
                span,
                ctx.source_file,
            ))
        }
    };
    Ok(res)
}

#[cfg(test)]
mod test;
