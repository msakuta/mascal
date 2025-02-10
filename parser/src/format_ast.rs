use crate::{
    parser::{ExprEnum, Expression, Statement},
    ArgDecl,
};

pub fn format_expr(
    ex: &Expression,
    level: usize,
    f: &mut impl std::io::Write,
) -> std::io::Result<()> {
    match &ex.expr {
        ExprEnum::NumLiteral(num, ts) => write!(f, "{num}: {ts}"),
        ExprEnum::StrLiteral(s) => write!(f, "\"{s}\""), // TODO: escape
        ExprEnum::Variable(name) => write!(f, "{name}"),
        ExprEnum::VarAssign(lhs, rhs) => {
            format_expr(lhs, level, f)?;
            write!(f, " = ")?;
            format_expr(rhs, level, f)?;
            Ok(())
        }
        ExprEnum::FnInvoke(fname, args) => {
            write!(f, "{fname}(")?;
            for (i, arg) in args.iter().enumerate() {
                format_expr(&arg.expr, level, f)?;
                if i != args.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")?;
            Ok(())
        }
        ExprEnum::Cast(ex, ty) => {
            format_expr(ex, level, f)?;
            write!(f, " as {ty}")?;
            Ok(())
        }
        ExprEnum::Neg(ex) => {
            write!(f, "-")?;
            format_expr(ex, level, f)?;
            Ok(())
        }
        ExprEnum::Add(lhs, rhs) => format_bin_op("+", lhs, rhs, level, f),
        ExprEnum::Sub(lhs, rhs) => format_bin_op("-", lhs, rhs, level, f),
        ExprEnum::Mult(lhs, rhs) => format_bin_op("*", lhs, rhs, level, f),
        ExprEnum::Div(lhs, rhs) => format_bin_op("/", lhs, rhs, level, f),
        ExprEnum::LT(lhs, rhs) => format_bin_op("<", lhs, rhs, level, f),
        ExprEnum::GT(lhs, rhs) => format_bin_op(">", lhs, rhs, level, f),
        ExprEnum::And(lhs, rhs) => format_bin_op("&&", lhs, rhs, level, f),
        ExprEnum::Or(lhs, rhs) => format_bin_op("||", lhs, rhs, level, f),
        ExprEnum::Not(ex) => {
            write!(f, "!")?;
            format_expr(ex, level, f)
        }
        ExprEnum::BitNot(expression) => {
            write!(f, "~")?;
            format_expr(ex, level, f)
        }
        ExprEnum::BitAnd(lhs, rhs) => format_bin_op("&", lhs, rhs, level, f),
        ExprEnum::BitXor(lhs, rhs) => format_bin_op("^", lhs, rhs, level, f),
        ExprEnum::BitOr(lhs, rhs) => format_bin_op("|", lhs, rhs, level, f),
        ExprEnum::Conditional(cond, t_branch, f_branch) => {
            let indent = "  ".repeat(level);
            write!(f, "if ")?;
            format_expr(cond, level, f)?;
            writeln!(f, " {{")?;
            for stmt in t_branch {
                format_stmt(stmt, level + 1, f)?;
            }
            write!(f, "{indent}}}")?;
            if let Some(stmts) = f_branch {
                writeln!(f, " else {{")?;
                for stmt in stmts {
                    format_stmt(stmt, level + 1, f)?;
                }
                write!(f, "{indent}}}")?;
            }
            Ok(())
        }
        ExprEnum::ArrLiteral(vec) => {
            write!(f, "[")?;
            for (i, elem) in vec.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                format_expr(elem, level + 1, f)?;
            }
            write!(f, "]")?;
            Ok(())
        }
        ExprEnum::TupleLiteral(vec) => {
            write!(f, "(")?;
            for elem in vec {
                format_expr(elem, level + 1, f)?;
            }
            write!(f, ")")?;
            Ok(())
        }
        ExprEnum::ArrIndex(arr, idx) => {
            format_expr(arr, level, f)?;
            write!(f, "[")?;
            for idx_elem in idx {
                format_expr(idx_elem, level + 1, f)?;
            }
            write!(f, "]")?;
            Ok(())
        }
        ExprEnum::TupleIndex(tup, idx) => {
            format_expr(tup, level, f)?;
            write!(f, ".{}", idx)?;
            Ok(())
        }
        ExprEnum::Brace(stmts) => {
            for stmt in stmts {
                format_stmt(stmt, level + 1, f)?;
            }
            Ok(())
        }
    }
}

fn format_bin_op(
    op: &str,
    lhs: &Expression,
    rhs: &Expression,
    level: usize,
    f: &mut impl std::io::Write,
) -> std::io::Result<()> {
    write!(f, "(")?;
    format_expr(lhs, level, f)?;
    write!(f, " {op} ")?;
    format_expr(rhs, level, f)?;
    write!(f, ")")?;
    Ok(())
}

pub fn format_params(params: &[ArgDecl], f: &mut impl std::io::Write) -> std::io::Result<()> {
    for (i, param) in params.iter().enumerate() {
        write!(f, "{}: {}", param.name, param.ty)?;
        if i != params.len() - 1 {
            write!(f, ", ")?;
        }
    }
    Ok(())
}

pub fn format_stmt(
    stmt: &Statement,
    level: usize,
    f: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let indent = "  ".repeat(level);
    match stmt {
        Statement::Comment(comment) => write!(f, "{comment}"),
        Statement::VarDecl(name, ty, init) => {
            write!(f, "{indent}var {name}: {ty} = ")?;
            if let Some(init) = init {
                format_expr(init, level, f)?;
            }
            writeln!(f, ";")
        }
        Statement::Expression(ex) => {
            write!(f, "{indent}")?;
            format_expr(ex, level, f)?;
            writeln!(f, ";")?;
            Ok(())
        }
        Statement::FnDecl {
            name,
            args,
            ret_type,
            stmts,
        } => {
            write!(f, "fn {}(", name)?;
            format_params(&args, f)?;
            writeln!(f, ") -> {} {{", ret_type)?;
            for stmt in stmts.iter() {
                format_stmt(stmt, level + 1, f)?;
            }
            writeln!(f, "}}")?;
            Ok(())
        }
        Statement::Loop(stmts) => {
            write!(f, "loop {{")?;
            for stmt in stmts {
                format_stmt(stmt, level, f)?;
            }
            write!(f, "}}")?;
            Ok(())
        }
        Statement::While(cond, stmts) => {
            write!(f, "while ")?;
            format_expr(cond, level, f)?;
            write!(f, "{{")?;
            for stmt in stmts {
                format_stmt(stmt, level, f)?;
            }
            write!(f, "}}")?;
            Ok(())
        }
        Statement::For(name, start, end, stmts) => {
            write!(f, "{indent}for {} in ", name)?;
            format_expr(&start, level, f)?;
            write!(f, " to ")?;
            format_expr(&end, level, f)?;
            writeln!(f, " {{")?;
            for stmt in stmts {
                format_stmt(stmt, level + 1, f)?;
            }
            writeln!(f, "{indent}}}")?;
            Ok(())
        }
        Statement::Break => write!(f, "{indent}break;"),
    }
}

pub fn format_stmts<'a>(
    stmts: &[Statement<'a>],
    f: &mut impl std::io::Write,
) -> std::io::Result<()> {
    for stmt in stmts {
        format_stmt(stmt, 0, f)?;
    }
    Ok(())
}
