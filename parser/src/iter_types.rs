use crate::{
    interpreter::RetType,
    parser::{ExprEnum, Expression, Statement},
    Span, TypeDecl,
};

/// Iterate AST and call the callback for each occurrence of type declaration.
/// Useful for inlay hinting inferred types.
pub fn iter_types(ast: &[Statement], f: &mut impl FnMut(Span, &TypeDecl)) {
    for stmt in ast {
        match stmt {
            Statement::FnDecl { stmts, .. } => {
                // Parameters are required to put type annotations for now.
                // for arg in args {
                //     f(arg.name, &arg.ty);
                // }
                iter_types(stmts.as_ref(), f);
            }
            Statement::VarDecl(name, ty, _init) => {
                f(*name, ty);
            }
            Statement::Expression { ex, .. } => {
                iter_types_expr(ex, f);
            }
            Statement::Comment(_) => (),
            Statement::Loop(stmts) => iter_types(stmts, f),
            Statement::While(cond, stmts) => {
                iter_types_expr(cond, f);
                iter_types(stmts, f);
            }
            Statement::For(_var, init, end, stmts) => {
                iter_types_expr(init, f);
                iter_types_expr(end, f);
                iter_types(stmts, f);
            }
            Statement::Break => (),
        }
    }
}

fn iter_types_expr(ex: &Expression, f: &mut impl FnMut(Span, &TypeDecl)) {
    match &ex.expr {
        ExprEnum::NumLiteral(_, type_set) => {
            if let Some(RetType::Some(ty)) = type_set.determine() {
                f(ex.span, &ty);
            }
        }
        ExprEnum::StrLiteral(_)
        | ExprEnum::ArrLiteral(_)
        | ExprEnum::TupleLiteral(_)
        | ExprEnum::Variable(_) => (),
        ExprEnum::Cast(ex, _) | ExprEnum::VarAssign(ex, _) => {
            iter_types_expr(ex, f);
        }
        ExprEnum::FnInvoke(_, args) => {
            for arg in args {
                iter_types_expr(&arg.expr, f);
            }
        }
        ExprEnum::ArrIndex(_, elems) => {
            for elem in elems {
                iter_types_expr(elem, f);
            }
        }
        ExprEnum::TupleIndex(ex, _)
        | ExprEnum::Not(ex)
        | ExprEnum::BitNot(ex)
        | ExprEnum::Neg(ex) => {
            iter_types_expr(ex, f);
        }
        ExprEnum::Add(lhs, rhs)
        | ExprEnum::Sub(lhs, rhs)
        | ExprEnum::Mult(lhs, rhs)
        | ExprEnum::Div(lhs, rhs)
        | ExprEnum::LT(lhs, rhs)
        | ExprEnum::GT(lhs, rhs)
        | ExprEnum::BitAnd(lhs, rhs)
        | ExprEnum::BitXor(lhs, rhs)
        | ExprEnum::BitOr(lhs, rhs)
        | ExprEnum::And(lhs, rhs)
        | ExprEnum::Or(lhs, rhs) => {
            iter_types_expr(lhs, f);
            iter_types_expr(rhs, f);
        }
        ExprEnum::Conditional(cond, t_branch, f_branch) => {
            iter_types_expr(cond, f);
            iter_types(t_branch, f);
            if let Some(f_branch) = f_branch {
                iter_types(f_branch, f);
            }
        }
        ExprEnum::Brace(stmts) => iter_types(stmts, f),
    }
}
