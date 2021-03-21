use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, none_of},
    combinator::{map_res, opt, recognize},
    multi::{fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::prelude::*;
use std::{cell::RefCell, collections::HashMap, env};

#[derive(Debug, PartialEq, Clone, Copy)]
enum TypeDecl {
    F64,
    F32,
    I64,
    I32,
    Str,
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    F64(f64),
    F32(f32),
    I64(i64),
    I32(i32),
    Str(String),
}

#[derive(Debug, PartialEq, Clone)]
struct ArgDecl<'a>(&'a str, TypeDecl);

#[derive(Debug, PartialEq, Clone)]
enum Statement<'a> {
    Comment(&'a str),
    VarDecl(&'a str, TypeDecl, Option<Expression<'a>>),
    FnDecl(&'a str, Vec<ArgDecl<'a>>, Vec<Statement<'a>>),
    Expression(Expression<'a>),
    Loop(Vec<Statement<'a>>),
    While(Expression<'a>, Vec<Statement<'a>>),
    For(&'a str, Expression<'a>, Expression<'a>, Vec<Statement<'a>>),
    Break,
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'a> {
    NumLiteral(Value),
    StrLiteral(String),
    Variable(&'a str),
    VarAssign(&'a str, Box<Expression<'a>>),
    FnInvoke(&'a str, Vec<Expression<'a>>),
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Sub(Box<Expression<'a>>, Box<Expression<'a>>),
    Mult(Box<Expression<'a>>, Box<Expression<'a>>),
    Div(Box<Expression<'a>>, Box<Expression<'a>>),
    LT(Box<Expression<'a>>, Box<Expression<'a>>),
    GT(Box<Expression<'a>>, Box<Expression<'a>>),
    And(Box<Expression<'a>>, Box<Expression<'a>>),
    Or(Box<Expression<'a>>, Box<Expression<'a>>),
    Conditional(
        Box<Expression<'a>>,
        Vec<Statement<'a>>,
        Option<Vec<Statement<'a>>>,
    ),
    Brace(Vec<Statement<'a>>),
}

fn comment(input: &str) -> IResult<&str, Statement> {
    let (r, _) = multispace0(input)?;
    delimited(tag("/*"), take_until("*/"), tag("*/"))(r).map(|(r, s)| (r, Statement::Comment(s)))
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn ident_space(input: &str) -> IResult<&str, &str> {
    delimited(multispace0, identifier, multispace0)(input)
}

fn var_ref(input: &str) -> IResult<&str, Expression> {
    let (r, res) = ident_space(input)?;
    Ok((r, Expression::Variable(res)))
}

fn type_spec(input: &str) -> IResult<&str, TypeDecl> {
    let (r, type_) = opt(delimited(
        delimited(multispace0, tag(":"), multispace0),
        alt((tag("f64"), tag("f32"), tag("i64"), tag("i32"), tag("str"))),
        multispace0,
    ))(input)?;
    Ok((
        r,
        match type_ {
            Some("f64") | None => TypeDecl::F64,
            Some("f32") => TypeDecl::F32,
            Some("i32") => TypeDecl::I32,
            Some("i64") => TypeDecl::I64,
            Some("str") => TypeDecl::Str,
            Some(unknown) => panic!(format!("Unknown type: \"{}\"", unknown)),
        },
    ))
}

fn var_decl(input: &str) -> IResult<&str, Statement> {
    let (r, _) = multispace1(tag("var")(multispace0(input)?.0)?.0)?;
    let (r, ident) = identifier(r)?;
    let (r, ts) = type_spec(r)?;
    let (r, initializer) = opt(delimited(
        delimited(multispace0, tag("="), multispace0),
        full_expression,
        multispace0,
    ))(r)?;
    let (r, _) = char(';')(multispace0(r)?.0)?;
    Ok((r, Statement::VarDecl(ident, ts, initializer)))
}

fn double_expr(input: &str) -> IResult<&str, Expression> {
    let (r, v) = recognize_float(input)?;
    // For now we have very simple conditinon to decide if it is a floating point literal
    // by a presense of a period.
    Ok((
        r,
        Expression::NumLiteral(if v.contains('.') {
            let parsed = v.parse().map_err(|_| {
                nom::Err::Error(nom::error::Error {
                    input,
                    code: nom::error::ErrorKind::Digit,
                })
            })?;
            Value::F64(parsed)
        } else {
            Value::I64(v.parse().map_err(|_| {
                nom::Err::Error(nom::error::Error {
                    input,
                    code: nom::error::ErrorKind::Digit,
                })
            })?)
        }),
    ))
}

fn numeric_literal_expression(input: &str) -> IResult<&str, Expression> {
    delimited(multispace0, double_expr, multispace0)(input)
}

fn str_literal(input: &str) -> IResult<&str, Expression> {
    let (r, val) = delimited(
        preceded(multispace0, char('\"')),
        many0(none_of("\"\\")),
        terminated(char('"'), multispace0),
    )(input)?;
    Ok((r, Expression::StrLiteral(val.iter().collect())))
}

// We parse any expr surrounded by parens, ignoring all whitespaces around those
fn parens(i: &str) -> IResult<&str, Expression> {
    delimited(
        multispace0,
        delimited(tag("("), conditional_expr, tag(")")),
        multispace0,
    )(i)
}

fn func_invoke(i: &str) -> IResult<&str, Expression> {
    let (r, ident) = delimited(multispace0, identifier, multispace0)(i)?;
    // println!("func_invoke ident: {}", ident);
    let (r, args) = delimited(
        multispace0,
        delimited(
            tag("("),
            many0(delimited(
                multispace0,
                expr,
                delimited(multispace0, opt(tag(",")), multispace0),
            )),
            tag(")"),
        ),
        multispace0,
    )(r)?;
    Ok((r, Expression::FnInvoke(ident, args)))
}

// We transform an double string into a Expression::NumLiteral
// on failure, we fallback to the parens parser defined above
fn factor(i: &str) -> IResult<&str, Expression> {
    alt((
        numeric_literal_expression,
        str_literal,
        func_invoke,
        var_ref,
        parens,
        brace_expr,
    ))(i)
}

// We read an initial factor and for each time we find
// a * or / operator followed by another factor, we do
// the math by folding everything
fn term(i: &str) -> IResult<&str, Expression> {
    let (i, init) = factor(i)?;

    fold_many0(
        pair(alt((char('*'), char('/'))), factor),
        init,
        |acc, (op, val): (char, Expression)| {
            if op == '*' {
                Expression::Mult(Box::new(acc), Box::new(val))
            } else {
                Expression::Div(Box::new(acc), Box::new(val))
            }
        },
    )(i)
}

fn expr(i: &str) -> IResult<&str, Expression> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(alt((char('+'), char('-'))), term),
        init,
        |acc, (op, val): (char, Expression)| {
            if op == '+' {
                Expression::Add(Box::new(acc), Box::new(val))
            } else {
                Expression::Sub(Box::new(acc), Box::new(val))
            }
        },
    )(i)
}

fn cmp(i: &str) -> IResult<&str, Expression> {
    let (i, lhs) = expr(i)?;

    let (i, (op, val)) = pair(alt((char('<'), char('>'))), expr)(i)?;
    Ok((
        i,
        if op == '<' {
            Expression::LT(Box::new(lhs), Box::new(val))
        } else {
            Expression::GT(Box::new(lhs), Box::new(val))
        },
    ))
}

fn conditional(i: &str) -> IResult<&str, Expression> {
    let (r, _) = delimited(multispace0, tag("if"), multispace0)(i)?;
    let (r, cond) = or_expr(r)?;
    let (r, true_branch) = delimited(
        delimited(multispace0, tag("{"), multispace0),
        source,
        delimited(multispace0, tag("}"), multispace0),
    )(r)?;
    let (r, false_branch) = opt(preceded(
        delimited(multispace0, tag("else"), multispace0),
        alt((
            delimited(
                delimited(multispace0, tag("{"), multispace0),
                source,
                delimited(multispace0, tag("}"), multispace0),
            ),
            map_res(
                conditional,
                |v| -> Result<Vec<Statement>, nom::error::Error<&str>> {
                    Ok(vec![Statement::Expression(v)])
                },
            ),
        )),
    ))(r)?;
    Ok((
        r,
        Expression::Conditional(Box::new(cond), true_branch, false_branch),
    ))
}

fn var_assign(input: &str) -> IResult<&str, Expression> {
    let (r, res) = tuple((ident_space, char('='), cmp_expr))(input)?;
    Ok((r, Expression::VarAssign(res.0, Box::new(res.2))))
}

fn cmp_expr(i: &str) -> IResult<&str, Expression> {
    alt((cmp, expr))(i)
}

fn and(i: &str) -> IResult<&str, Expression> {
    let (r, first) = cmp_expr(i)?;
    let (r, _) = delimited(multispace0, tag("&&"), multispace0)(r)?;
    let (r, second) = cmp_expr(r)?;
    Ok((r, Expression::And(Box::new(first), Box::new(second))))
}

fn and_expr(i: &str) -> IResult<&str, Expression> {
    alt((and, cmp_expr))(i)
}

fn or(i: &str) -> IResult<&str, Expression> {
    let (r, first) = and_expr(i)?;
    let (r, _) = delimited(multispace0, tag("||"), multispace0)(r)?;
    let (r, second) = and_expr(r)?;
    Ok((r, Expression::Or(Box::new(first), Box::new(second))))
}

fn or_expr(i: &str) -> IResult<&str, Expression> {
    alt((or, and_expr))(i)
}

fn assign_expr(i: &str) -> IResult<&str, Expression> {
    alt((var_assign, or_expr))(i)
}

fn conditional_expr(i: &str) -> IResult<&str, Expression> {
    alt((conditional, assign_expr))(i)
}

fn brace_expr(input: &str) -> IResult<&str, Expression> {
    let (r, v) = delimited(
        delimited(multispace0, tag("{"), multispace0),
        source,
        delimited(multispace0, tag("}"), multispace0),
    )(input)?;
    Ok((r, Expression::Brace(v)))
}

fn full_expression(input: &str) -> IResult<&str, Expression> {
    conditional_expr(input)
}

fn expression_statement(input: &str) -> IResult<&str, Statement> {
    let (r, val) = full_expression(input)?;
    Ok((r, Statement::Expression(val)))
}

fn func_arg(input: &str) -> IResult<&str, ArgDecl> {
    let (r, v) = pair(
        identifier,
        opt(delimited(multispace0, type_spec, multispace0)),
    )(input)?;
    Ok((r, ArgDecl(v.0, v.1.unwrap_or(TypeDecl::F64))))
}

fn func_decl(input: &str) -> IResult<&str, Statement> {
    let (r, _) = multispace1(tag("fn")(multispace0(input)?.0)?.0)?;
    let (r, ident) = identifier(r)?;
    let (r, args) = delimited(
        multispace0,
        delimited(
            tag("("),
            many0(delimited(
                multispace0,
                func_arg,
                delimited(multispace0, opt(tag(",")), multispace0),
            )),
            tag(")"),
        ),
        multispace0,
    )(r)?;
    let (r, stmts) = delimited(
        delimited(multispace0, tag("{"), multispace0),
        source,
        delimited(multispace0, tag("}"), multispace0),
    )(r)?;
    Ok((r, Statement::FnDecl(ident, args, stmts)))
}

fn loop_stmt(input: &str) -> IResult<&str, Statement> {
    let (r, _) = multispace0(tag("loop")(multispace0(input)?.0)?.0)?;
    let (r, stmts) = delimited(
        delimited(multispace0, tag("{"), multispace0),
        source,
        delimited(multispace0, tag("}"), multispace0),
    )(r)?;
    Ok((r, Statement::Loop(stmts)))
}

fn while_stmt(input: &str) -> IResult<&str, Statement> {
    let (r, _) = multispace0(tag("while")(multispace0(input)?.0)?.0)?;
    let (r, cond) = cmp_expr(r)?;
    let (r, stmts) = delimited(
        delimited(multispace0, tag("{"), multispace0),
        source,
        delimited(multispace0, tag("}"), multispace0),
    )(r)?;
    Ok((r, Statement::While(cond, stmts)))
}

fn for_stmt(input: &str) -> IResult<&str, Statement> {
    let (r, _) = delimited(multispace0, tag("for"), multispace1)(input)?;
    let (r, iter) = identifier(r)?;
    let (r, _) = delimited(multispace0, tag("in"), multispace0)(r)?;
    let (r, from) = expr(r)?;
    let (r, _) = delimited(multispace0, tag(".."), multispace0)(r)?;
    let (r, to) = expr(r)?;
    let (r, stmts) = delimited(
        delimited(multispace0, tag("{"), multispace0),
        source,
        delimited(multispace0, tag("}"), multispace0),
    )(r)?;
    Ok((r, Statement::For(iter, from, to, stmts)))
}

fn break_stmt(input: &str) -> IResult<&str, Statement> {
    let (r, _) = delimited(multispace0, tag("break"), multispace0)(input)?;
    Ok((r, Statement::Break))
}

fn general_statement<'a>(last: bool) -> impl Fn(&'a str) -> IResult<&'a str, Statement> {
    let terminator = move |i| -> IResult<&str, ()> {
        let mut semicolon = pair(tag(";"), multispace0);
        if last {
            Ok((opt(semicolon)(i)?.0, ()))
        } else {
            Ok((semicolon(i)?.0, ()))
        }
    };
    move |input: &str| {
        alt((
            var_decl,
            func_decl,
            loop_stmt,
            while_stmt,
            for_stmt,
            terminated(break_stmt, terminator),
            terminated(expression_statement, terminator),
            comment,
        ))(input)
    }
}

fn last_statement(input: &str) -> IResult<&str, Statement> {
    general_statement(true)(input)
}

fn statement(input: &str) -> IResult<&str, Statement> {
    general_statement(false)(input)
}

fn source(input: &str) -> IResult<&str, Vec<Statement>> {
    let (r, mut v) = many0(statement)(input)?;
    let (r, last) = opt(last_statement)(r)?;
    if let Some(last) = last {
        v.push(last);
    }
    Ok((r, v))
}

macro_rules! unwrap_run {
    ($e:expr) => {
        match $e {
            RunResult::Yield(v) => v,
            RunResult::Break => return RunResult::Break,
        }
    };
}

fn binary_op(
    lhs: Value,
    rhs: Value,
    d: impl Fn(f64, f64) -> f64,
    i: impl Fn(i64, i64) -> i64,
) -> Value {
    match (lhs.clone(), rhs.clone()) {
        (Value::F64(lhs), rhs) => Value::F64(d(lhs, coerce_f64(&rhs))),
        (lhs, Value::F64(rhs)) => Value::F64(d(coerce_f64(&lhs), rhs)),
        (Value::F32(lhs), rhs) => Value::F32(d(lhs as f64, coerce_f64(&rhs)) as f32),
        (lhs, Value::F32(rhs)) => Value::F32(d(coerce_f64(&lhs), rhs as f64) as f32),
        (Value::I64(lhs), Value::I64(rhs)) => Value::I64(i(lhs, rhs)),
        (Value::I64(lhs), Value::I32(rhs)) => Value::I64(i(lhs, rhs as i64)),
        (Value::I32(lhs), Value::I64(rhs)) => Value::I64(i(lhs as i64, rhs)),
        (Value::I32(lhs), Value::I32(rhs)) => Value::I32(i(lhs as i64, rhs as i64) as i32),
        _ => panic!(format!(
            "Unsupported addition between {:?} and {:?}",
            lhs, rhs
        )),
    }
}

fn truthy(a: &Value) -> bool {
    match *a {
        Value::F64(v) => v != 0.,
        Value::F32(v) => v != 0.,
        Value::I64(v) => v != 0,
        Value::I32(v) => v != 0,
        _ => false,
    }
}

fn coerce_f64(a: &Value) -> f64 {
    match *a {
        Value::F64(v) => v as f64,
        Value::F32(v) => v as f64,
        Value::I64(v) => v as f64,
        Value::I32(v) => v as f64,
        _ => 0.,
    }
}

fn coerce_i64(a: &Value) -> i64 {
    match *a {
        Value::F64(v) => v as i64,
        Value::F32(v) => v as i64,
        Value::I64(v) => v as i64,
        Value::I32(v) => v as i64,
        _ => 0,
    }
}

fn coerce_str(a: &Value) -> String {
    match a {
        Value::F64(v) => v.to_string(),
        Value::F32(v) => v.to_string(),
        Value::I64(v) => v.to_string(),
        Value::I32(v) => v.to_string(),
        Value::Str(v) => v.clone(),
    }
}

fn coerce_var(value: &Value, target: &Value) -> Value {
    match target {
        Value::F64(_) => Value::F64(coerce_f64(value)),
        Value::F32(_) => Value::F32(coerce_f64(value) as f32),
        Value::I64(_) => Value::I64(coerce_i64(value)),
        Value::I32(_) => Value::I32(coerce_i64(value) as i32),
        Value::Str(_) => Value::Str(coerce_str(value)),
    }
}

fn coerce_type(value: &Value, target: &TypeDecl) -> Value {
    match target {
        TypeDecl::F64 => Value::F64(coerce_f64(value)),
        TypeDecl::F32 => Value::F32(coerce_f64(value) as f32),
        TypeDecl::I64 => Value::I64(coerce_i64(value)),
        TypeDecl::I32 => Value::I32(coerce_i64(value) as i32),
        TypeDecl::Str => Value::Str(coerce_str(value)),
    }
}

fn eval<'a, 'b>(e: &'b Expression<'a>, ctx: &mut EvalContext<'a, 'b, '_, '_>) -> RunResult {
    match e {
        Expression::NumLiteral(val) => RunResult::Yield(val.clone()),
        Expression::StrLiteral(val) => RunResult::Yield(Value::Str(val.clone())),
        Expression::Variable(str) => RunResult::Yield(
            ctx.get_var(str)
                .expect(&format!("Variable {} not found in scope", str)),
        ),
        Expression::VarAssign(str, rhs) => {
            let mut value = unwrap_run!(eval(rhs, ctx));
            let mut search_ctx: Option<&EvalContext> = Some(ctx);
            while let Some(c) = search_ctx {
                let existing_value = if let Some(val) = c.variables.borrow().get(str) {
                    val.clone()
                } else {
                    search_ctx = c.super_context;
                    continue;
                };
                value = coerce_var(&value, &existing_value);
                c.variables.borrow_mut().insert(str, value.clone());
                break;
            }
            if search_ctx.is_none() {
                panic!(format!("Variable \"{}\" was not declared!", str));
            }
            RunResult::Yield(value)
        }
        Expression::FnInvoke(str, args) => {
            let args = args.iter().map(|v| eval(v, ctx)).collect::<Vec<_>>();
            let mut subctx = EvalContext::push_stack(ctx);
            let func = ctx
                .get_fn(*str)
                .expect(&format!("function {} is not defined.", str));
            match func {
                FuncDef::Code(func) => {
                    for (k, v) in func.args.iter().zip(&args) {
                        subctx
                            .variables
                            .borrow_mut()
                            .insert(k.0, coerce_type(unwrap_run!(v), &k.1));
                    }
                    let run_result = run(func.stmts, &mut subctx).unwrap();
                    match run_result {
                        RunResult::Yield(v) => RunResult::Yield(v),
                        RunResult::Break => panic!("break in function toplevel"),
                    }
                }
                FuncDef::Native(native) => RunResult::Yield(native(
                    &args
                        .iter()
                        .map(|e| {
                            if let RunResult::Yield(v) = e {
                                v.clone()
                            } else {
                                Value::F64(0.)
                            }
                        })
                        .collect::<Vec<_>>(),
                )),
            }
        }
        Expression::Add(lhs, rhs) => {
            let res = RunResult::Yield(binary_op(
                unwrap_run!(eval(lhs, ctx)),
                unwrap_run!(eval(rhs, ctx)),
                |lhs, rhs| lhs + rhs,
                |lhs, rhs| lhs + rhs,
            ));
            res
        }
        Expression::Sub(lhs, rhs) => RunResult::Yield(binary_op(
            unwrap_run!(eval(lhs, ctx)),
            unwrap_run!(eval(rhs, ctx)),
            |lhs, rhs| lhs - rhs,
            |lhs, rhs| lhs - rhs,
        )),
        Expression::Mult(lhs, rhs) => RunResult::Yield(binary_op(
            unwrap_run!(eval(lhs, ctx)),
            unwrap_run!(eval(rhs, ctx)),
            |lhs, rhs| lhs * rhs,
            |lhs, rhs| lhs * rhs,
        )),
        Expression::Div(lhs, rhs) => RunResult::Yield(binary_op(
            unwrap_run!(eval(lhs, ctx)),
            unwrap_run!(eval(rhs, ctx)),
            |lhs, rhs| lhs / rhs,
            |lhs, rhs| lhs / rhs,
        )),
        Expression::LT(lhs, rhs) => RunResult::Yield(binary_op(
            unwrap_run!(eval(lhs, ctx)),
            unwrap_run!(eval(rhs, ctx)),
            |lhs, rhs| if lhs < rhs { 1. } else { 0. },
            |lhs, rhs| if lhs < rhs { 1 } else { 0 },
        )),
        Expression::GT(lhs, rhs) => RunResult::Yield(binary_op(
            unwrap_run!(eval(lhs, ctx)),
            unwrap_run!(eval(rhs, ctx)),
            |lhs, rhs| if lhs > rhs { 1. } else { 0. },
            |lhs, rhs| if lhs > rhs { 1 } else { 0 },
        )),
        Expression::And(lhs, rhs) => RunResult::Yield(Value::I32(
            if truthy(&unwrap_run!(eval(lhs, ctx))) && truthy(&unwrap_run!(eval(rhs, ctx))) {
                1
            } else {
                0
            },
        )),
        Expression::Or(lhs, rhs) => RunResult::Yield(Value::I32(
            if truthy(&unwrap_run!(eval(lhs, ctx))) || truthy(&unwrap_run!(eval(rhs, ctx))) {
                1
            } else {
                0
            },
        )),
        Expression::Conditional(cond, true_branch, false_branch) => {
            if truthy(&unwrap_run!(eval(cond, ctx))) {
                run(true_branch, ctx).unwrap()
            } else if let Some(ast) = false_branch {
                run(ast, ctx).unwrap()
            } else {
                RunResult::Yield(Value::I32(0))
            }
        }
        Expression::Brace(stmts) => {
            let mut subctx = EvalContext::push_stack(ctx);
            run(stmts, &mut subctx).unwrap()
        }
    }
}

fn s_print(vals: &[Value]) -> Value {
    if let [val, ..] = vals {
        match val {
            Value::F64(val) => println!("print: {}", val),
            Value::F32(val) => println!("print: {}", val),
            Value::I64(val) => println!("print: {}", val),
            Value::I32(val) => println!("print: {}", val),
            Value::Str(val) => println!("print: {}", val),
        }
    }
    Value::I32(0)
}

fn s_puts(vals: &[Value]) -> Value {
    if let [val, ..] = vals {
        match val {
            Value::F64(val) => print!("print: {}", val),
            Value::F32(val) => print!("print: {}", val),
            Value::I64(val) => print!("print: {}", val),
            Value::I32(val) => print!("print: {}", val),
            Value::Str(val) => print!("print: {}", val),
        }
    }
    Value::I32(0)
}

fn s_type(vals: &[Value]) -> Value {
    if let [val, ..] = vals {
        Value::Str(match val {
            Value::F64(_) => "f64".to_string(),
            Value::F32(_) => "f32".to_string(),
            Value::I64(_) => "i64".to_string(),
            Value::I32(_) => "i32".to_string(),
            Value::Str(_) => "str".to_string(),
        })
    } else {
        Value::I32(0)
    }
}

#[derive(Clone)]
struct FuncCode<'src, 'ast> {
    args: &'ast Vec<ArgDecl<'src>>,
    stmts: &'ast Vec<Statement<'src>>,
}

#[derive(Clone)]
enum FuncDef<'src, 'ast, 'native> {
    Code(FuncCode<'src, 'ast>),
    Native(&'native dyn Fn(&[Value]) -> Value),
}

/// A context stat for evaluating a script.
///
/// It has 2 lifetime arguments, one for the source code ('src) and the other for
/// the AST ('ast), because usually AST is created after the source.
#[derive(Clone)]
struct EvalContext<'src, 'ast, 'native, 'ctx> {
    /// RefCell to allow mutation in super context
    variables: RefCell<HashMap<&'src str, Value>>,
    /// Function names are owned strings because it can be either from source or native.
    /// Unlike variables, functions cannot be overwritten in the outer scope, so it does not
    /// need to be wrapped in a RefCell.
    functions: HashMap<String, FuncDef<'src, 'ast, 'native>>,
    super_context: Option<&'ctx EvalContext<'src, 'ast, 'native, 'ctx>>,
}

impl<'src, 'ast, 'native, 'ctx> EvalContext<'src, 'ast, 'native, 'ctx> {
    fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert("print".to_string(), FuncDef::Native(&s_print));
        functions.insert("puts".to_string(), FuncDef::Native(&s_puts));
        functions.insert("type".to_string(), FuncDef::Native(&s_type));
        Self {
            variables: RefCell::new(HashMap::new()),
            functions,
            super_context: None,
        }
    }

    fn push_stack(super_ctx: &'ctx Self) -> Self {
        Self {
            variables: RefCell::new(HashMap::new()),
            functions: HashMap::new(),
            super_context: Some(super_ctx),
        }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.variables.borrow_mut().get(name) {
            Some(val.clone())
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_var(name)
        } else {
            None
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FuncDef<'src, 'ast, 'native>> {
        if let Some(val) = self.functions.get(name) {
            Some(val)
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_fn(name)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq)]
enum RunResult {
    Yield(Value),
    Break,
}

macro_rules! unwrap_break {
    ($e:expr) => {
        match $e {
            RunResult::Yield(v) => v,
            RunResult::Break => break,
        }
    };
}

fn run<'src, 'ast>(
    stmts: &'ast Vec<Statement<'src>>,
    ctx: &mut EvalContext<'src, 'ast, '_, '_>,
) -> Result<RunResult, ()> {
    let mut res = RunResult::Yield(Value::I32(0));
    for stmt in stmts {
        match stmt {
            Statement::VarDecl(var, type_, initializer) => {
                let init_val = if let Some(init_expr) = initializer {
                    unwrap_break!(eval(init_expr, ctx))
                } else {
                    Value::I32(0)
                };
                let init_val = match type_ {
                    TypeDecl::F64 => Value::F64(coerce_f64(&init_val)),
                    TypeDecl::F32 => Value::F32(coerce_f64(&init_val) as f32),
                    TypeDecl::I64 => Value::I64(coerce_i64(&init_val)),
                    TypeDecl::I32 => Value::I32(coerce_i64(&init_val) as i32),
                    TypeDecl::Str => Value::Str(coerce_str(&init_val)),
                };
                ctx.variables.borrow_mut().insert(*var, init_val);
            }
            Statement::FnDecl(var, args, stmts) => {
                ctx.functions
                    .insert(var.to_string(), FuncDef::Code(FuncCode { args, stmts }));
            }
            Statement::Expression(e) => {
                res = eval(&e, ctx);
                if let RunResult::Break = res {
                    return Ok(res);
                }
                // println!("Expression evaluates to: {:?}", res);
            }
            Statement::Loop(e) => loop {
                res = match run(e, ctx)? {
                    RunResult::Yield(v) => RunResult::Yield(v),
                    RunResult::Break => break,
                };
            },
            Statement::While(cond, e) => loop {
                match eval(cond, ctx) {
                    RunResult::Yield(v) => {
                        if truthy(&v) {
                            break;
                        }
                    }
                    RunResult::Break => break,
                }
                res = match run(e, ctx)? {
                    RunResult::Yield(v) => RunResult::Yield(v),
                    RunResult::Break => break,
                };
            },
            Statement::For(iter, from, to, e) => {
                let from_res = coerce_i64(&unwrap_break!(eval(from, ctx))) as i64;
                let to_res = coerce_i64(&unwrap_break!(eval(to, ctx))) as i64;
                for i in from_res..to_res {
                    ctx.variables.borrow_mut().insert(iter, Value::I64(i));
                    res = match run(e, ctx)? {
                        RunResult::Yield(v) => RunResult::Yield(v),
                        RunResult::Break => break,
                    };
                }
            }
            Statement::Break => {
                return Ok(RunResult::Break);
            }
            _ => {}
        }
    }
    Ok(res)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut contents = String::new();
    let code = if 1 < args.len() {
        if let Ok(mut file) = File::open(&args[1]) {
            file.read_to_string(&mut contents)?;
            &contents
        } else {
            &args[1]
        }
    } else {
        r"var x;
  /* This is a block comment. */
  var y;
  123;
  123 + 456;
  "
    };
    if let Ok(result) = source(code) {
        println!("Match: {:?}", result.1);
        run(&result.1, &mut EvalContext::new()).expect("Error in run()");
    } else {
        println!("failed");
    }
    Ok(())
}

mod test;
