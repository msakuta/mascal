//! Type inferer/checker implementation.
//! Type inference and type checking is mostly the same, except that type inference can actively
//! modify the AST to resolve missing type annotations.
//! We name this module after inference because it represents the value more.

use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    format_ast::{format_expr, format_stmt},
    interpreter::{std_functions, FuncCode, RetType, TypeMapRc},
    parser::{ExprEnum, Expression, Statement, StructDecl},
    type_decl::{ArraySize, TypeDecl},
    type_set::TypeSet,
    FuncDef, Span,
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

impl<'src> std::error::Error for TypeCheckError<'src> {}

impl<'src> TypeCheckError<'src> {
    fn new(msg: impl Into<String>, span: Span<'src>, source_file: Option<&'src str>) -> Self {
        Self {
            msg: msg.into(),
            span,
            source_file,
        }
    }

    pub fn span(&self) -> Span<'src> {
        self.span
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

    pub(crate) fn indeterminant_type(span: Span<'src>, source_file: Option<&'src str>) -> Self {
        Self::new(format!("type could not be determined"), span, source_file)
    }

    pub(crate) fn void_value(span: Span<'src>, source_file: Option<&'src str>) -> Self {
        Self::new(
            format!("a value of type void is instantiated"),
            span,
            source_file,
        )
    }

    pub(crate) fn undefined_type(
        name: Span<'src>,
        span: Span<'src>,
        source_file: Option<&'src str>,
    ) -> Self {
        Self::new(format!("type {} is not defined", name), span, source_file)
    }

    pub(crate) fn undefined_field(name: Span<'src>, source_file: Option<&'src str>) -> Self {
        Self::new(format!("field {} is not defined", name), name, source_file)
    }
}

/// A type information about a variable in the type inference context.
/// The difference between parameter and non-parameter (locally declared variables)
/// is that the former cannot be modified.
#[derive(Clone, Default, Debug)]
pub struct VariableType {
    parameter: bool,
    ts: TypeSet,
}

impl VariableType {
    fn parameter(ts: TypeSet) -> Self {
        Self {
            parameter: true,
            ts,
        }
    }

    fn local(ts: TypeSet) -> Self {
        Self {
            parameter: false,
            ts,
        }
    }
}

impl std::fmt::Display for VariableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ts.fmt(f)
    }
}

#[derive(Clone)]
pub struct TypeCheckContext<'src, 'native, 'ctx> {
    /// Variables table for type checking.
    pub(crate) variables: HashMap<&'src str, VariableType>,
    /// Function names are owned strings because it can be either from source or native.
    functions: HashMap<String, FuncDef<'src, 'native>>,
    super_context: Option<&'ctx TypeCheckContext<'src, 'native, 'ctx>>,
    source_file: Option<&'src str>,
    pub(crate) typedefs: TypeMapRc<'src>,
}

impl<'src, 'native, 'ctx> TypeCheckContext<'src, 'native, 'ctx> {
    pub fn new(source_file: Option<&'src str>) -> Self {
        Self {
            variables: HashMap::new(),
            functions: std_functions(),
            super_context: None,
            source_file,
            typedefs: HashMap::new(),
        }
    }

    fn get_var(&self, name: &str) -> Option<VariableType> {
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

    fn get_type(&self, name: &str) -> Option<&Rc<StructDecl<'src>>> {
        self.typedefs
            .get(name)
            .or_else(|| self.super_context.and_then(|sc| sc.get_type(name)))
    }

    fn push_stack(super_ctx: &'ctx Self) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            super_context: Some(super_ctx),
            source_file: super_ctx.source_file,
            typedefs: HashMap::new(),
        }
    }

    fn set_var_type(&mut self, var_ref: &VarRef<'src>, ty: &TypeSet) {
        if let Some(var_ty) = self.get_var_mut(var_ref) {
            *var_ty = ty.clone();
        }
    }

    fn intersect_var_type(&mut self, var_ref: &VarRef<'src>, ty: &TypeSet) -> Result<(), String> {
        if let Some(var_ty) = self.get_var_mut(var_ref) {
            *var_ty = var_ty.try_intersect(ty)?;
        }
        Ok(())
    }

    /// Peel off variable reference to constrain the type set
    fn get_var_mut(&mut self, var_ref: &VarRef<'src>) -> Option<&mut TypeSet> {
        match var_ref {
            VarRef::Variable(name) => {
                // We do not get mutable reference to super context, since we do not support inter-function
                // type inference (yet).
                let entry = self.variables.entry(name).or_default();
                // Parameter type cannot be inferred. It should be given in the declaration.
                if entry.parameter {
                    None
                } else {
                    Some(&mut entry.ts)
                }
            }
            VarRef::Array(int) => {
                if let Some(var) = self.get_var_mut(int) {
                    var.and_then_mut(|v| v.array.as_mut())
                        .map(|(var_int, _)| &mut *var_int.as_mut())
                } else {
                    None
                }
            }
            VarRef::Tuple(int, idx) => {
                if let Some(var) = self.get_var_mut(int) {
                    var.and_then_mut(|v| v.tuple.as_mut())
                        .and_then(|var_int| var_int.get_mut(*idx))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

pub(crate) fn tc_expr_forward<'src, 'b, 'native>(
    e: &'b Expression<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<TypeSet, TypeCheckError<'src>>
where
    'native: 'src,
{
    macro_rules! tc_bail {
        ($msg:expr, $ex:expr) => {
            return Err(TypeCheckError::new($msg, $ex.span, ctx.source_file));
        };
    }

    Ok(match &e.expr {
        ExprEnum::NumLiteral(_, ts) => ts.ts.clone(),
        ExprEnum::StrLiteral(_val) => TypeSet::str(),
        ExprEnum::ArrLiteral(val) => {
            if val.is_empty() {
                return Ok(TypeSet::array(TypeSet::all(), ArraySize::Fixed(0)));
            }
            for (ex1, ex2) in val[..val.len() - 1].iter().zip(val[1..].iter()) {
                let el1 = tc_expr_forward(ex1, ctx)?;
                let el2 = tc_expr_forward(ex2, ctx)?;
                if el1 != el2 {
                    return Err(TypeCheckError::new(
                        format!("Types in an array is not homogeneous: {el1:?} and {el2:?}"),
                        e.span,
                        ctx.source_file,
                    ));
                }
            }
            let ty = val
                .first()
                .map(|e| tc_expr_forward(e, ctx))
                .unwrap_or(Ok(TypeSet::all()))?;
            TypeSet::array(ty, ArraySize::Fixed(val.len()))
        }
        ExprEnum::TupleLiteral(val) => {
            let type_sets = val
                .iter()
                .map(|e| tc_expr_forward(e, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            TypeSet::tuple(type_sets)
        }
        ExprEnum::Variable(str) => {
            ctx.get_var(str)
                .ok_or_else(|| TypeCheckError::undefined_var(str, e.span, ctx.source_file))?
                .ts
        }
        ExprEnum::Cast(_ex, decl) => decl.into(),
        ExprEnum::VarAssign(lhs, rhs) => {
            let span = lhs.span;
            if let Some((var_ref, decl_ty)) = forward_lvalue(lhs, ctx)
                .map_err(|err| TypeCheckError::new(err, span, ctx.source_file))?
            {
                let rhs_ty = tc_expr_forward(rhs, ctx)?;
                let ty = decl_ty
                    .clone()
                    .try_intersect(&rhs_ty)
                    .map_err(|err| TypeCheckError::new(err, span, ctx.source_file))?;
                ctx.set_var_type(&var_ref, &ty);
            }

            binary_op(&lhs, &rhs, e.span, ctx, "Assignment")?
        }
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
                    *ty_arg = Some(tc_expr_forward(&arg.expr, ctx)?);
                }
            }

            // Find and assign named args
            for arg in args.iter() {
                if let Some(name) = arg.name {
                    if let Some(ty_arg) = fn_args
                        .iter()
                        .enumerate()
                        .find(|f| *f.1.name == *name)
                        .and_then(|(i, _)| ty_args.get_mut(i))
                    {
                        *ty_arg = Some(tc_expr_forward(&arg.expr, ctx)?);
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
                let ty_arg = ty_arg.as_ref().ok_or_else(|| {
                    TypeCheckError::unassigned_arg(*decl.name, e.span, ctx.source_file)
                })?;
                tc_coerce_type(&ty_arg, &decl.ty, e.span, ctx)?;
            }

            let func = ctx
                .get_fn(*fname)
                .ok_or_else(|| TypeCheckError::undefined_fn(fname, e.span, ctx.source_file))?;
            match func {
                FuncDef::Code(code) => (&code.ret_type).into(),
                FuncDef::Native(native) => native
                    .ret_type
                    .as_ref()
                    .map_or(TypeSet::void(), |v| v.into()),
            }
        }
        ExprEnum::ArrIndex(ex, args) => {
            let arg_types = args
                .iter()
                .map(|v| tc_expr_forward(v, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let _arg0 = {
                let v = arg_types[0].clone();
                let coerced_ty = tc_coerce_type(&v, &TypeDecl::I64, args[0].span, ctx)?;
                if TypeSet::i64() == coerced_ty {
                    v
                } else {
                    return Err(TypeCheckError::new(
                        "Subscript type should be integer types".to_string(),
                        args.first().unwrap_or(e).span,
                        ctx.source_file,
                    ));
                }
            };
            let res = tc_expr_forward(ex, ctx)?;
            if let Some((inner, _)) = res.and_then(|ts| ts.array.as_ref()) {
                *inner.clone()
            } else {
                tc_bail!(
                    format!(
                        "Subscript operator's first operand is not an array but {}",
                        res
                    ),
                    ex
                );
            }
        }
        ExprEnum::TupleIndex(ex, index) => {
            let result = tc_expr_forward(ex, ctx)?;
            if let Some(tuple) = result.and_then(|s| s.tuple.as_ref()) {
                tuple
                    .get(*index)
                    .ok_or_else(|| {
                        TypeCheckError::new("Tuple index out of range", ex.span, ctx.source_file)
                    })?
                    .clone()
            } else {
                tc_bail!("Tuple index applied to a non-tuple", ex);
            }
        }
        ExprEnum::FieldAccess {
            prefix,
            postfix: field_name,
            ..
        } => {
            let result = tc_expr_forward(prefix, ctx)?;

            let Some(RetType::Some(ty)) = result.determine() else {
                tc_bail!("Field access operator must have a determined type", prefix);
            };

            let TypeDecl::TypeName(tn) = ty else {
                tc_bail!(
                    "Field access operator must have a struct operand on left hand side",
                    prefix
                );
            };

            let Some(st_ty) = ctx.get_type(&tn) else {
                tc_bail!(format!("Type name {tn} not found"), prefix);
            };

            let Some(field) = st_ty
                .fields
                .iter()
                .find(|field| *field.name == **field_name)
            else {
                tc_bail!(format!("Field name {field_name} not found"), prefix);
            };

            TypeSet::from(&field.ty)
        }
        ExprEnum::Not(val) => {
            tc_expr_forward(val, ctx)?;
            // The result of logical operator should be i32 (bool)
            TypeDecl::I32.into()
        }
        ExprEnum::BitNot(val) => tc_expr_forward(val, ctx)?,
        ExprEnum::Neg(val) => tc_expr_forward(val, ctx)?,
        ExprEnum::Add(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Add")?,
        ExprEnum::Sub(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Sub")?,
        ExprEnum::Mult(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Mult")?,
        ExprEnum::Div(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Div")?,
        ExprEnum::LT(lhs, rhs) => binary_cmp(&lhs, &rhs, e.span, ctx, "LT")?,
        ExprEnum::LE(lhs, rhs) => binary_cmp(&lhs, &rhs, e.span, ctx, "LE")?,
        ExprEnum::GT(lhs, rhs) => binary_cmp(&lhs, &rhs, e.span, ctx, "GT")?,
        ExprEnum::GE(lhs, rhs) => binary_cmp(&lhs, &rhs, e.span, ctx, "GE")?,
        ExprEnum::EQ(lhs, rhs) => binary_cmp(&lhs, &rhs, e.span, ctx, "EQ")?,
        ExprEnum::NE(lhs, rhs) => binary_cmp(&lhs, &rhs, e.span, ctx, "NE")?,
        ExprEnum::BitAnd(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "BitAnd")?,
        ExprEnum::BitXor(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "BitXor")?,
        ExprEnum::BitOr(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "BitOr")?,
        ExprEnum::And(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "And")?,
        ExprEnum::Or(lhs, rhs) => binary_op(&lhs, &rhs, e.span, ctx, "Or")?,
        ExprEnum::Conditional(cond, true_branch, false_branch) => {
            tc_coerce_type(&tc_expr_forward(cond, ctx)?, &TypeDecl::I32, cond.span, ctx)?;
            let true_type = tc_stmts_forward(true_branch, ctx)?;
            if let Some(false_type) = false_branch {
                let false_type = tc_stmts_forward(false_type, ctx)?;
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
            tc_stmts_forward(stmts, &mut subctx)?
        }
        ExprEnum::StructLiteral { name, fields, .. } => {
            let struct_decl = ctx
                .get_type(**name)
                .ok_or_else(|| TypeCheckError::undefined_type(*name, e.span, ctx.source_file))?
                .clone();
            for (field_name, ex) in fields {
                let forward_ty = tc_expr_forward(ex, ctx)?;
                let field = struct_decl
                    .fields
                    .iter()
                    .find(|field| *field.name == **field_name)
                    .ok_or_else(|| TypeCheckError::undefined_field(*field_name, ctx.source_file))?;
                let ts: TypeSet = TypeSet::from(&field.ty);
                ts.try_intersect(&forward_ty)
                    .map_err(|err| TypeCheckError::new(err, ex.span, ctx.source_file))?;
            }
            TypeSet::type_name(name.to_string())
        }
    })
}

fn tc_expr_reverse<'src, 'b, 'native>(
    e: &'b mut Expression<'src>,
    ts: &TypeSet,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<(), TypeCheckError<'src>>
where
    'native: 'src,
{
    let span = e.span;
    match &mut e.expr {
        ExprEnum::NumLiteral(_, target_ts) => target_ts.ts = ts.clone(),
        ExprEnum::StrLiteral(_) => (), // String literals always of type string, so nothing to propagate
        ExprEnum::ArrLiteral(vec) => {
            if let Some((arr_ts, size)) = ts.and_then(|ts| ts.array.as_ref()) {
                if !size.contains(vec.len()) {
                    return Err(TypeCheckError::new(
                        format!(
                            "Size is not compatible: {} is not contained in {}",
                            vec.len(),
                            size
                        ),
                        span,
                        ctx.source_file,
                    ));
                }
                for elem in vec {
                    tc_expr_reverse(elem, arr_ts, ctx)?;
                }
            }
        }
        ExprEnum::TupleLiteral(vec) => {
            if let Some(tuple) = ts.and_then(|ts| ts.tuple.as_ref()) {
                if tuple.len() != vec.len() {
                    return Err(TypeCheckError::new(
                        format!(
                            "Tuple sizes are not the same: {} != {}",
                            tuple.len(),
                            vec.len()
                        ),
                        span,
                        ctx.source_file,
                    ));
                }
                for (target, source) in vec.iter_mut().zip(tuple.iter()) {
                    tc_expr_reverse(target, source, ctx)?;
                }
            }
        }
        ExprEnum::StructLiteral { name, fields, def } => {
            let span = e.span;

            // TODO: work around clone() for the borrow checker
            let struct_decl = ctx
                .get_type(**name)
                .ok_or_else(|| TypeCheckError::undefined_type(*name, span, ctx.source_file))?
                .clone();

            *def = Some(struct_decl.clone());

            if let Some(tuple) = ts.and_then(|ts| ts.tuple.as_ref()) {
                if tuple.len() != fields.len() {
                    return Err(TypeCheckError::new(
                        format!(
                            "Tuple sizes are not the same: {} != {}",
                            tuple.len(),
                            fields.len()
                        ),
                        span,
                        ctx.source_file,
                    ));
                }
                for (field_name, field) in fields.iter_mut() {
                    let decl = struct_decl
                        .fields
                        .iter()
                        .find(|field_decl| *field_decl.name == **field_name)
                        .ok_or_else(|| {
                            TypeCheckError::undefined_field(*field_name, ctx.source_file)
                        })?;
                    tc_expr_reverse(field, &TypeSet::from(&decl.ty), ctx)?;
                }
            }
        }
        ExprEnum::Variable(name) => {
            let source_file = ctx.source_file;
            ctx.intersect_var_type(&VarRef::Variable(name), ts)
                .map_err(|e| TypeCheckError::new(e, span, source_file))?;
        }
        ExprEnum::Cast(ex, _ty) => tc_expr_reverse(ex, &TypeSet::all(), ctx)?,
        ExprEnum::VarAssign(lhs, rhs) => {
            tc_expr_reverse(lhs, ts, ctx)?;
            tc_expr_reverse(rhs, &tc_expr_forward(lhs, ctx)?, ctx)?
        }
        ExprEnum::FnInvoke(fname, args) => {
            let fn_decl = ctx
                .get_fn(*fname)
                .ok_or_else(|| TypeCheckError::undefined_fn(fname, span, ctx.source_file))?;
            let params = fn_decl.args().clone();
            for (i, arg) in args.iter_mut().enumerate().rev() {
                let ty = params
                    .get(i)
                    .map_or(TypeDecl::Any, |param| param.ty.clone());
                tc_expr_reverse(&mut arg.expr, &(&ty).into(), ctx)?;
            }
        }
        ExprEnum::ArrIndex(ex, indices) => {
            tc_expr_reverse(ex, &TypeSet::array(ts.clone(), ArraySize::Any), ctx)?;
            for idx in indices {
                // For now, array indices are always integers
                tc_expr_reverse(idx, &TypeSet::int(), ctx)?;
            }
        }
        ExprEnum::TupleIndex(ex, idx) => {
            // This is not the most efficient method to update an element in a tuple.
            // It searches existing tuple type yielded by the subexpression and replace the item given
            // by the caller.
            // TODO: We could propagate path (VecRef) to update the element by tc_expr_propagate,
            // for a better performance.
            if let Some(existing_tuple) = tc_expr_forward(ex, ctx)?.and_then(|v| v.tuple.as_ref()) {
                let mut altered_tuple = existing_tuple.clone();
                altered_tuple[*idx] = altered_tuple[*idx]
                    .try_intersect(ts)
                    .map_err(|e| TypeCheckError::new(e, span, ctx.source_file))?;
                tc_expr_reverse(ex, &TypeSet::tuple(altered_tuple), ctx)?;
            }
        }
        ExprEnum::FieldAccess { prefix, def, .. } => {
            tc_expr_reverse(prefix, &TypeSet::Any, ctx)?;

            let ty = tc_expr_forward(prefix, ctx)?
                .determine()
                .ok_or_else(|| TypeCheckError::indeterminant_type(prefix.span, ctx.source_file))?
                .ok_or_else(|| TypeCheckError::indeterminant_type(prefix.span, ctx.source_file))?;

            let TypeDecl::TypeName(name) = ty else {
                return Err(TypeCheckError::indeterminant_type(
                    prefix.span,
                    ctx.source_file,
                ));
            };

            // TODO: work around clone() for the borrow checker
            let struct_decl = ctx
                .get_type(&name)
                .ok_or_else(|| TypeCheckError::undefined_type(prefix.span, span, ctx.source_file))?
                .clone();

            *def = Some(struct_decl.clone());
        }
        ExprEnum::Not(ex) | ExprEnum::BitNot(ex) | ExprEnum::Neg(ex) => {
            tc_expr_reverse(ex, ts, ctx)?;
        }
        ExprEnum::Add(lhs, rhs)
        | ExprEnum::Sub(lhs, rhs)
        | ExprEnum::Mult(lhs, rhs)
        | ExprEnum::Div(lhs, rhs)
        | ExprEnum::BitAnd(lhs, rhs)
        | ExprEnum::BitXor(lhs, rhs)
        | ExprEnum::BitOr(lhs, rhs)
        | ExprEnum::And(lhs, rhs)
        | ExprEnum::Or(lhs, rhs) => {
            tc_expr_reverse(lhs, ts, ctx)?;
            tc_expr_reverse(rhs, ts, ctx)?;
        }
        ExprEnum::LT(lhs, rhs)
        | ExprEnum::LE(lhs, rhs)
        | ExprEnum::GT(lhs, rhs)
        | ExprEnum::GE(lhs, rhs)
        | ExprEnum::EQ(lhs, rhs)
        | ExprEnum::NE(lhs, rhs) => {
            // Comparison operators yield I32 regardless of operand types, so reverse propagation
            // loses the original type information. Therefore, we need to forward from either side
            // and propagate to the other side. It has possibility of exponential growth in number
            // of evaluations, but in reality you won't nest comparison expressions (you may chain it
            // with logical && or ||, but they won't invoke exponential growth).
            // TODO: reduce duplicate evaluation
            let lhs_ts = tc_expr_forward(lhs, ctx)?;
            let rhs_ts = tc_expr_forward(rhs, ctx)?;
            let inter_ts = lhs_ts.try_intersect(&rhs_ts).unwrap();
            tc_expr_reverse(lhs, &inter_ts, ctx)?;
            tc_expr_reverse(rhs, &inter_ts, ctx)?;
        }
        ExprEnum::Conditional(cond, t_branch, f_branch) => {
            tc_expr_reverse(cond, &TypeSet::i32(), ctx)?;
            tc_stmts_reverse(t_branch, ts, ctx)?;
            if let Some(f_branch) = f_branch {
                tc_stmts_reverse(f_branch, ts, ctx)?;
            }
        }
        ExprEnum::Brace(stmts) => {
            let mut subctx = TypeCheckContext::push_stack(ctx);
            tc_stmts_forward(stmts, &mut subctx)?;
            tc_stmts_reverse(stmts, ts, &mut subctx)?
        }
    }
    Ok(())
}

/// An abstract type that represents a path to a part of type constraint.
/// For example, if you have a variable and type `var a: [x]` where x is unknown,
/// `a[0] = 1 as i32` will constrain `x` to be i32.
/// We need a way to represent this information, starting from the variable name,
/// and the chain of suffixes.
#[derive(Debug)]
enum VarRef<'src> {
    Variable(&'src str),
    Array(Box<VarRef<'src>>),
    Tuple(Box<VarRef<'src>>, usize),
    /// Struct fields have definite type by definition, so there is no need to constrain
    /// their types with type inference.
    Struct,
}

/// Try to evaluate expression as an lvalue and return its variable name and TypeSet,
/// or None if it doesn't yield an lvalue.
fn forward_lvalue<'src, 'b, 'native>(
    ex: &'b Expression<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<Option<(VarRef<'src>, TypeSet)>, String> {
    match &ex.expr {
        ExprEnum::NumLiteral(_, _) => Err("Numeric literal cannot be a lvalue".to_string()),
        ExprEnum::StrLiteral(_) => Err("Literal string cannot be a lvalue".to_string()),
        ExprEnum::ArrLiteral(_) => Err("Array literal cannot be a lvalue".to_string()),
        ExprEnum::TupleLiteral(_) => Err("Tuple literal cannot be a lvalue".to_string()),
        ExprEnum::StructLiteral { .. } => Err("Struct literal cannot be a lvalue".to_string()),
        ExprEnum::Variable(name) => {
            let var_type = ctx
                .get_var(name)
                .ok_or_else(|| format!("Variable {name} not found"))?;
            Ok(Some((VarRef::Variable(name), var_type.ts.clone())))
        }
        ExprEnum::VarAssign(_lhs, _rhs) => {
            // Variable assign expression yields rvalue by convention. For example, `(a = b) = c` won't work, because
            // `a = b` is not an lvalue. However, `a = b = c` will work, since it is parsed as `a = (b = c)` and the
            // left hand side is an lvalue for both assignments.
            // This is the same for C (gcc gives "lvalue required as left operand of assignment" error in the former case).
            Ok(None)
        }
        ExprEnum::FnInvoke(_, _) => Err("Function return value cannot be a lvalue".to_string()),
        ExprEnum::Cast(_, _) => Err("Cast expression cannot be a lvalue".to_string()),
        ExprEnum::ArrIndex(ex, _idx) => {
            let prior = forward_lvalue(&ex, ctx)?;
            if let Some((var_ref, ts)) = prior {
                if let Some(arr) = ts.and_then(|ts| ts.array.as_ref()) {
                    Ok(Some((VarRef::Array(Box::new(var_ref)), *arr.0.clone())))
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        }
        ExprEnum::TupleIndex(ex, idx) => {
            let prior = forward_lvalue(&ex, ctx)?;
            if let Some((var_ref, ts)) = prior {
                if let Some(tuple) = ts.and_then(|ts| ts.tuple.as_ref()) {
                    Ok(tuple
                        .get(*idx)
                        .map(|v| (VarRef::Tuple(Box::new(var_ref), *idx), v.clone())))
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        }
        ExprEnum::FieldAccess {
            prefix: ex,
            postfix: field_name,
            ..
        } => {
            let prior = forward_lvalue(&ex, ctx)?;
            if let Some((_var_ref, ts)) = prior {
                let ty = ts
                    .determine()
                    .ok_or_else(|| {
                        format!(
                            "TypeCheckError::indeterminant_type({}, {:?})",
                            ex.span, ctx.source_file
                        )
                    })?
                    .ok_or_else(|| {
                        format!(
                            "TypeCheckError::void_value({}, {:?})",
                            ex.span, ctx.source_file
                        )
                    })?;
                if let TypeDecl::TypeName(type_name) = ty {
                    let st_ty = ctx.get_type(&type_name).ok_or_else(|| {
                        format!(
                            "TypeCheckError::struct not found({}, {:?})",
                            ex.span, ctx.source_file
                        )
                    })?;
                    let field_ty = st_ty
                        .fields
                        .iter()
                        .find(|field| *field.name == **field_name)
                        .ok_or_else(|| format!("field {} not found", field_name))?;
                    // We make sure that the struct and the field exists, but we do not apply type constraints by lvalue,
                    // because field types should be determined without inference.
                    Ok(Some((VarRef::Struct, TypeSet::from(&field_ty.ty))))
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        }
        ExprEnum::Neg(_)
        | ExprEnum::Add(_, _)
        | ExprEnum::Sub(_, _)
        | ExprEnum::Mult(_, _)
        | ExprEnum::Div(_, _)
        | ExprEnum::LT(_, _)
        | ExprEnum::LE(_, _)
        | ExprEnum::GT(_, _)
        | ExprEnum::GE(_, _)
        | ExprEnum::EQ(_, _)
        | ExprEnum::NE(_, _)
        | ExprEnum::And(_, _)
        | ExprEnum::Or(_, _)
        | ExprEnum::Not(_)
        | ExprEnum::BitAnd(_, _)
        | ExprEnum::BitOr(_, _)
        | ExprEnum::BitXor(_, _)
        | ExprEnum::BitNot(_)
        | ExprEnum::Conditional(_, _, _) => {
            Err("Arithmetic expression cannot be a lvalue".to_string())
        }
        ExprEnum::Brace(_) => Err("Brace expression cannot be a lvalue".to_string()),
    }
}

fn tc_coerce_type<'src>(
    value: &TypeSet,
    target: &TypeDecl,
    span: Span<'src>,
    ctx: &TypeCheckContext<'src, '_, '_>,
) -> Result<TypeSet, TypeCheckError<'src>> {
    let target: TypeSet = target.into();
    let res = value
        .try_intersect(&target)
        .map_err(|err| TypeCheckError::new(err, span, ctx.source_file))?;
    Ok(res)
}

fn tc_stmt_forward<'src, 'ast, 'native>(
    stmt: &'ast Statement<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<TypeSet, TypeCheckError<'src>>
where
    'native: 'src,
{
    let mut res = TypeSet::all();
    match stmt {
        Statement::VarDecl {
            name: var,
            ty,
            init,
            ..
        } => {
            if matches!(ty, TypeDecl::Any) {
                let init_type = if let Some(init_expr) = init {
                    let mut buf = vec![0u8; 0];
                    format_expr(init_expr, 0, &mut buf).unwrap();
                    let init_type = tc_expr_forward(init_expr, ctx)?;
                    tc_coerce_type(&init_type, ty, init_expr.span, ctx)?
                } else {
                    ty.into()
                };
                ctx.variables
                    .insert(**var, VariableType::local(init_type.into()));
            } else {
                if let Some(init_expr) = init {
                    let init_type = tc_expr_forward(init_expr, ctx)?;
                    tc_coerce_type(&init_type, ty, init_expr.span, ctx)?;
                }
                // If the declaration has a type declaraction, we respect it.
                // Namely, don't fix a type of `var a: [i32] = [1, 2, 3]` to `[i32; 3]` because
                // we may want to push to this array later.
                ctx.variables.insert(**var, VariableType::local(ty.into()));
            }
            res = TypeSet::void();
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
                    let init_ty =
                        tc_expr_forward(init, &mut TypeCheckContext::new(ctx.source_file))?;
                    tc_coerce_type(
                        &init_ty,
                        &arg.ty,
                        Span::new(name),
                        &mut TypeCheckContext::new(ctx.source_file),
                    )?;
                }
                subctx
                    .variables
                    .insert(*arg.name, VariableType::parameter(arg.ty.clone().into()));
            }
            let last_stmt = tc_stmts_forward(stmts, &mut subctx)?;
            if let Some(Statement::Expression {
                ex: ret_expr,
                semicolon: false,
            }) = stmts.last()
            {
                if let RetType::Some(ret_type) = ret_type {
                    tc_coerce_type(&last_stmt, &ret_type, ret_expr.span, ctx)?;
                } else if !last_stmt.is_void() {
                    return Err(TypeCheckError::new(
                        "Function with void return type returned some value".to_string(),
                        ret_expr.span,
                        ctx.source_file,
                    ));
                }
            }
            res = TypeSet::void();
        }
        Statement::Expression { ex, semicolon } => {
            let expr_res = tc_expr_forward(&ex, ctx)?;
            if !semicolon {
                res = expr_res;
            } else {
                res = TypeSet::void();
            }
        }
        Statement::Loop(e) => {
            res = tc_stmts_forward(e, ctx)?;
        }
        Statement::While(cond, e) => {
            tc_coerce_type(&tc_expr_forward(cond, ctx)?, &TypeDecl::I32, cond.span, ctx).map_err(
                |e| {
                    TypeCheckError::new(
                        format!("Type error in condition: {e}"),
                        cond.span,
                        ctx.source_file,
                    )
                },
            )?;
            res = tc_stmts_forward(e, ctx)?;
        }
        Statement::For {
            var,
            start,
            end,
            stmts,
            ..
        } => {
            let det_ty = tc_expr_forward(start, ctx)?
                .try_intersect(&tc_expr_forward(end, ctx)?)
                .map_err(|e| TypeCheckError::new(e, *var, ctx.source_file))?;
            ctx.variables.insert(var, VariableType::local(det_ty));
            res = tc_stmts_forward(stmts, ctx)?;
        }
        Statement::Break => {
            // TODO: check types in break out site. For now we disallow break with values like Rust.
        }
        // Struct should be definitely typed, until we have generic types.
        Statement::Struct(st) => {
            ctx.typedefs
                .insert(st.name.to_string(), Rc::new(st.clone()));
        }
        Statement::Comment(_) => (),
    }
    Ok(res)
}

fn tc_stmts_forward<'src, 'ast, 'native>(
    stmts: &'ast [Statement<'src>],
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<TypeSet, TypeCheckError<'src>>
where
    'native: 'src,
{
    let mut res = TypeSet::all();
    for stmt in stmts {
        res = tc_stmt_forward(stmt, ctx)?;
    }
    Ok(res)
}

pub fn tc_stmt_reverse<'src, 'ast, 'native>(
    stmt: &'ast mut Statement<'src>,
    ts: &TypeSet,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<(), TypeCheckError<'src>>
where
    'native: 'src,
{
    match stmt {
        Statement::VarDecl {
            name: var,
            ty: decl_type,
            init,
            ..
        } => {
            let propagating_type = ctx
                .variables
                .get(**var)
                .ok_or_else(|| TypeCheckError::undefined_var(var, *var, ctx.source_file))?
                .ts
                .determine()
                .ok_or_else(|| TypeCheckError::indeterminant_type(*var, ctx.source_file))?;
            let propagating_type_set = (&propagating_type).into();
            *decl_type = propagating_type
                .ok_or_else(|| TypeCheckError::void_value(*var, ctx.source_file))?;
            if let Some(initializer) = init {
                tc_expr_reverse(initializer, &propagating_type_set, ctx)?;
            }
        }
        Statement::FnDecl { .. } => {}
        Statement::Expression { ex: e, .. } => {
            tc_expr_reverse(e, ts, ctx)?;
        }
        Statement::Loop(stmts) => {
            tc_stmts_reverse(stmts, &TypeSet::default(), ctx)?;
        }
        Statement::While(cond, stmts) => {
            tc_stmts_reverse(stmts, &TypeSet::default(), ctx)?;
            tc_expr_reverse(cond, &TypeSet::i32(), ctx)?;
        }
        Statement::For {
            var,
            ty,
            start,
            end,
            stmts,
        } => {
            tc_stmts_reverse(stmts, &TypeSet::default(), ctx)?;
            let Some(idx_ty) = ctx.variables.get(**var) else {
                return Err(TypeCheckError::new(
                    format!("Could not find variable {}", var),
                    *var,
                    ctx.source_file,
                ));
            };
            dbg_println!("propagate For {}: {}", var, idx_ty);
            let idx_ty = idx_ty.ts.clone();
            let idx_det_ty = match idx_ty.determine() {
                Some(v) => v,
                None => idx_ty
                    .try_intersect(&TypeSet::i64())
                    .map_err(|e| TypeCheckError::new(e, *var, ctx.source_file))?
                    .determine()
                    .ok_or_else(|| TypeCheckError::indeterminant_type(*var, ctx.source_file))?,
            };
            *ty =
                Some(idx_det_ty.ok_or_else(|| TypeCheckError::void_value(*var, ctx.source_file))?);
            tc_expr_reverse(end, &idx_ty, ctx)?;
            tc_expr_reverse(start, &idx_ty, ctx)?;
        }
        Statement::Break => {
            // TODO: check types in break out site. For now we disallow break with values like Rust.
        }
        // Struct should be definitely typed, until we have generic types.
        Statement::Struct(_) | Statement::Comment(_) => (),
    }
    Ok(())
}

pub fn tc_stmts_reverse<'src, 'ast, 'native>(
    stmts: &'ast mut Vec<Statement<'src>>,
    ts: &TypeSet,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<(), TypeCheckError<'src>>
where
    'native: 'src,
{
    let mut iter = stmts.iter_mut().rev();
    if let Some(stmt) = iter.next() {
        tc_stmt_reverse(stmt, ts, ctx)?;
    }
    for stmt in iter {
        tc_stmt_reverse(stmt, &TypeSet::default(), ctx)?;
    }
    Ok(())
}

/// Type check and infer the given AST. Despite the name, the stmts will be changed by the type inference.
pub fn type_check<'src, 'ast, 'native>(
    stmts: &'ast mut Vec<Statement<'src>>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
) -> Result<(), TypeCheckError<'src>>
where
    'native: 'src,
{
    for stmt in stmts.iter_mut() {
        match stmt {
            Statement::FnDecl {
                name,
                args,
                ret_type,
                stmts,
            } => {
                ctx.functions.insert(
                    name.to_string(),
                    FuncDef::Code(FuncCode::new(stmts.clone(), args.clone(), ret_type.clone())),
                );
            }
            _ => {}
        }
    }

    for stmt in stmts.iter_mut() {
        match stmt {
            Statement::FnDecl {
                name,
                args,
                ret_type,
                stmts,
            } => {
                let mut inferer = TypeCheckContext::push_stack(ctx);
                inferer.variables = args
                    .iter()
                    .map(|param| {
                        if matches!(param.ty, TypeDecl::Any) {
                            Err(TypeCheckError::indeterminant_type(*name, ctx.source_file))
                        } else {
                            Ok((
                                *param.name,
                                VariableType::parameter(param.ty.clone().into()),
                            ))
                        }
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?;
                let mut last_ty = TypeSet::default();
                let stmts = Rc::make_mut(stmts);
                for stmt in stmts.iter_mut() {
                    let mut bytes = vec![];
                    if let Some(s) = format_stmt(stmt, 0, &mut bytes)
                        .ok()
                        .and_then(|_| String::from_utf8(bytes).ok())
                    {
                        dbg_println!("stmt before {s}");
                    }
                    last_ty = tc_stmt_forward(stmt, &mut inferer)?;
                    let mut bytes = vec![];
                    if let Some(s) = format_stmt(stmt, 0, &mut bytes)
                        .ok()
                        .and_then(|_| String::from_utf8(bytes).ok())
                    {
                        dbg_println!("stmt ty {last_ty}: {}", s);
                    } else {
                        dbg_println!("stmt ty {last_ty}");
                    }
                }
                let ret_type_set = (&*ret_type).into();
                let intersection = last_ty
                    .try_intersect(&ret_type_set)
                    .map_err(|e| TypeCheckError::new(e, *name, ctx.source_file))?;
                if let Some(_determined_ty) = intersection.determine() {
                    // For now, we require the return type to be fully annotated.
                    // We may have partial return type inference like `impl Trait` in Rust in the future,
                    // but not now.
                    //
                    // ret_type = determined_ts;
                    dbg_println!(
                        "Function {}'s return type is inferred to be {}",
                        name,
                        ret_type
                    );
                    tc_stmts_reverse(stmts, &ret_type_set, &mut inferer)?;
                } else if matches!(ret_type, RetType::Void) {
                    dbg_println!("Function {} returns void; coercing", name);
                    tc_stmts_reverse(stmts, &ret_type_set, &mut inferer)?;
                } else {
                    return Err(TypeCheckError::new(
                        format!(
                            "Function {}'s return type could not be determined: {}",
                            name, intersection
                        ),
                        *name,
                        ctx.source_file,
                    ));
                }
            }
            Statement::Struct(st) => {
                ctx.typedefs
                    .insert(st.name.to_string(), Rc::new(st.clone()));
            }
            _ => {}
        }
    }
    tc_stmts_forward(stmts, ctx)?;
    tc_stmts_reverse(stmts, &TypeSet::all(), ctx)?;
    Ok(())
}

fn binary_op_gen<'src, 'ast, 'native>(
    lhs: &'ast Expression<'src>,
    rhs: &'ast Expression<'src>,
    span: Span<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
    op: &str,
    mut f: impl FnMut(
        &TypeSet,
        &TypeSet,
        Span<'src>,
        &TypeCheckContext<'src, 'native, '_>,
    ) -> Result<TypeSet, TypeCheckError<'src>>,
) -> Result<TypeSet, TypeCheckError<'src>>
where
    'native: 'src,
{
    let lhst = tc_expr_forward(lhs, ctx)?;
    let rhst = tc_expr_forward(rhs, ctx)?;
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
) -> Result<TypeSet, TypeCheckError<'src>>
where
    'native: 'src,
{
    binary_op_gen(lhs, rhs, span, ctx, op, binary_op_type)
}

fn binary_op_type<'src>(
    lhs: &TypeSet,
    rhs: &TypeSet,
    span: Span<'src>,
    ctx: &TypeCheckContext<'src, '_, '_>,
) -> Result<TypeSet, TypeCheckError<'src>> {
    let map_err = |err| TypeCheckError::new(err, span, ctx.source_file);
    let res = lhs.try_intersect(rhs).map_err(map_err)?;
    if res.is_none() {
        return Err(map_err("Binary operation incompatible".to_string()));
    }
    Ok(res)
}

fn binary_cmp<'src, 'ast, 'native>(
    lhs: &'ast Expression<'src>,
    rhs: &'ast Expression<'src>,
    span: Span<'src>,
    ctx: &mut TypeCheckContext<'src, 'native, '_>,
    op: &str,
) -> Result<TypeSet, TypeCheckError<'src>>
where
    'native: 'src,
{
    binary_op_gen(lhs, rhs, span, ctx, op, binary_cmp_type)
}

/// Binary comparison operator type check. It will always return i32, which is used as a bool in this language.
fn binary_cmp_type<'src>(
    lhs: &TypeSet,
    rhs: &TypeSet,
    span: Span<'src>,
    ctx: &TypeCheckContext<'src, '_, '_>,
) -> Result<TypeSet, TypeCheckError<'src>> {
    lhs.try_intersect(rhs).map_err(|e| {
        TypeCheckError::new(
            format!(
                "Comparison between incompatible types: {} and {}: {e}",
                lhs, rhs
            ),
            span,
            ctx.source_file,
        )
    })?;
    Ok(TypeSet::i32())
}

#[cfg(test)]
mod test;
