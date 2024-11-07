use std::collections::HashMap;

use crate::parser::{ExprEnum, Expression, Statement};

type NameRangeMap = HashMap<String, (u32, u32)>;

/// Measures complexity of a source code by a metric I invented.
/// If the statements include a function definition, it will measure the function body
/// and sum up to the result.
pub fn complexity(ast: &[Statement]) -> usize {
    let mut names = NameRangeMap::new();
    let sum = stmts_complexity(ast, &mut names);
    sum + names
        .iter()
        .fold(0, |acc, (_, val)| acc + (val.1 - val.0) as usize)
}

fn stmts_complexity(ast: &[Statement], names: &mut NameRangeMap) -> usize {
    let mut sum = 0;
    for stmt in ast {
        match stmt {
            Statement::VarDecl(name, _ty, init) => {
                let entry = names
                    .entry(name.to_string())
                    .or_insert_with(|| (name.location_line(), name.location_line()));
                entry.0 = entry.0.min(name.location_line());
                entry.1 = entry.1.max(name.location_line());
                if let Some(init) = init {
                    sum += expr_complexity(init, names);
                }
            }
            Statement::Expression(ex) => {
                sum += expr_complexity(ex, names);
            }
            Statement::FnDecl {
                name, args, stmts, ..
            } => {
                let mut names = NameRangeMap::new();
                let line = name.location_line();
                for arg in args {
                    names.insert(arg.name.to_string(), (line, line));
                }
                sum += stmts_complexity(stmts, &mut names);
                sum += names
                    .iter()
                    .fold(0, |acc, (_, val)| acc + (val.1 - val.0) as usize);
            }
            Statement::For(var, start, end, stmts) => {
                let line = var.location_line();
                names.entry(var.to_string()).or_insert((line, line));
                sum += expr_complexity(start, names);
                sum += expr_complexity(end, names);
                sum += stmts_complexity(stmts, names);
            }
            Statement::Loop(stmts) => {
                sum += stmts_complexity(stmts, names);
            }
            Statement::While(cond, stmts) => {
                sum += expr_complexity(cond, names);
                sum += stmts_complexity(stmts, names);
            }
            _ => {}
        }
    }
    sum
}

fn expr_complexity(ex: &Expression, names: &mut NameRangeMap) -> usize {
    match &ex.expr {
        ExprEnum::NumLiteral(_)
        | ExprEnum::StrLiteral(_)
        | ExprEnum::ArrLiteral(_)
        | ExprEnum::TupleLiteral(_) => 0,
        ExprEnum::Variable(name) => {
            let line = ex.span.location_line();
            let entry = names
                .entry(name.to_string())
                .or_insert_with(|| (line, line));
            entry.0 = entry.0.min(line);
            entry.1 = entry.1.max(line);
            0
        }
        ExprEnum::Cast(ex, _) => expr_complexity(ex, names),
        ExprEnum::FnInvoke(name, args) => {
            let entry = names.entry(name.to_string()).or_default();
            entry.0 = entry.0.min(ex.span.location_line());
            entry.1 = entry.1.max(ex.span.location_line());
            args.iter()
                .map(|arg| expr_complexity(&arg.expr, names))
                .sum()
        }
        ExprEnum::ArrIndex(ex, indices) => {
            expr_complexity(ex, names)
                + indices
                    .iter()
                    .map(|idx| expr_complexity(idx, names))
                    .sum::<usize>()
        }
        ExprEnum::TupleIndex(ex, _) => expr_complexity(ex, names),
        ExprEnum::VarAssign(lhs, rhs)
        | ExprEnum::Add(lhs, rhs)
        | ExprEnum::Sub(lhs, rhs)
        | ExprEnum::Mult(lhs, rhs)
        | ExprEnum::Div(lhs, rhs)
        | ExprEnum::LT(lhs, rhs)
        | ExprEnum::GT(lhs, rhs)
        | ExprEnum::BitAnd(lhs, rhs)
        | ExprEnum::BitOr(lhs, rhs)
        | ExprEnum::BitXor(lhs, rhs)
        | ExprEnum::And(lhs, rhs)
        | ExprEnum::Or(lhs, rhs) => expr_complexity(lhs, names) + expr_complexity(rhs, names),
        ExprEnum::Not(ex) | ExprEnum::BitNot(ex) | ExprEnum::Neg(ex) => expr_complexity(ex, names),
        ExprEnum::Conditional(cond, t_branch, f_branch) => {
            expr_complexity(cond, names)
                + stmts_complexity(t_branch, names)
                + f_branch
                    .as_ref()
                    .map_or(0, |stmts| stmts_complexity(stmts, names))
        }
        ExprEnum::Brace(stmts) => stmts_complexity(stmts, names),
    }
}