use crate::{parser::Statement, Span, TypeDecl};

/// Iterate AST and call the callback for each occurrence of type declaration.
/// Useful for inlay hinting inferred types.
pub fn iter_types(ast: &[Statement], f: &mut impl FnMut(Span, &TypeDecl)) {
    for stmt in ast {
        match stmt {
            Statement::FnDecl { stmts, .. } => iter_types(stmts.as_ref(), f),
            Statement::VarDecl(name, ty, _init) => {
                f(*name, ty);
            }
            _ => {}
        }
    }
}
