use crate::{
    coercion::{coerce_f32, coerce_f64, coerce_i32, coerce_i64},
    type_decl::{ArraySize, TypeDecl},
    type_set::TypeSet,
    Value,
};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{
        alpha1, alphanumeric1, char, digit1, multispace0, multispace1, none_of, one_of,
    },
    combinator::{map_res, opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, InputTake, Offset,
};
use nom_locate::LocatedSpan;
use std::{rc::Rc, string::FromUtf8Error};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub enum ReadError {
    IO(std::io::Error),
    FromUtf8(FromUtf8Error),
    NoMainFound,
    UndefinedOpCode(u8),
    UnknownTag([u8; 2]),
    UnknownTypeTag(u8),
}

impl From<std::io::Error> for ReadError {
    fn from(e: std::io::Error) -> Self {
        ReadError::IO(e)
    }
}

impl From<FromUtf8Error> for ReadError {
    fn from(e: FromUtf8Error) -> Self {
        ReadError::FromUtf8(e)
    }
}

impl std::fmt::Display for ReadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadError::IO(e) => write!(f, "{e}"),
            ReadError::FromUtf8(e) => write!(f, "{e}"),
            ReadError::NoMainFound => write!(f, "No main function found"),
            ReadError::UndefinedOpCode(code) => write!(f, "Opcode \"{code:02X}\" unrecognized!"),
            ReadError::UnknownTag(code) => write!(f, "Unknwon tag \"{code:?}\" encountered"),
            ReadError::UnknownTypeTag(code) => {
                write!(f, "Unknwon type tag \"{code:?}\" encountered")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArgDecl<'a> {
    pub name: Span<'a>,
    pub ty: TypeDecl,
    pub init: Option<Expression<'a>>,
}

impl<'a> ArgDecl<'a> {
    pub fn new(name: impl Into<Span<'a>>, ty: TypeDecl) -> Self {
        Self {
            name: name.into(),
            ty,
            init: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Comment(&'a str),
    VarDecl(Span<'a>, TypeDecl, Option<Expression<'a>>),
    FnDecl {
        name: Span<'a>,
        args: Vec<ArgDecl<'a>>,
        ret_type: TypeSet,
        stmts: Rc<Vec<Statement<'a>>>,
    },
    Expression {
        ex: Expression<'a>,
        semicolon: bool,
    },
    Loop(Vec<Statement<'a>>),
    While(Expression<'a>, Vec<Statement<'a>>),
    For(Span<'a>, Expression<'a>, Expression<'a>, Vec<Statement<'a>>),
    Break,
}

impl<'a> Statement<'a> {
    fn expects_semicolon(&self) -> bool {
        matches!(
            self,
            Self::Expression {
                semicolon: false,
                ..
            } | Self::Break
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ExprEnum<'a> {
    NumLiteral(Value, TypeSet),
    StrLiteral(String),
    ArrLiteral(Vec<Expression<'a>>),
    TupleLiteral(Vec<Expression<'a>>),
    Variable(&'a str),
    Cast(Box<Expression<'a>>, TypeDecl),
    VarAssign(Box<Expression<'a>>, Box<Expression<'a>>),
    FnInvoke(&'a str, Vec<FnArg<'a>>),
    ArrIndex(Box<Expression<'a>>, Vec<Expression<'a>>),
    TupleIndex(Box<Expression<'a>>, usize),
    Not(Box<Expression<'a>>),
    BitNot(Box<Expression<'a>>),
    Neg(Box<Expression<'a>>),
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Sub(Box<Expression<'a>>, Box<Expression<'a>>),
    Mult(Box<Expression<'a>>, Box<Expression<'a>>),
    Div(Box<Expression<'a>>, Box<Expression<'a>>),
    LT(Box<Expression<'a>>, Box<Expression<'a>>),
    GT(Box<Expression<'a>>, Box<Expression<'a>>),
    BitAnd(Box<Expression<'a>>, Box<Expression<'a>>),
    BitXor(Box<Expression<'a>>, Box<Expression<'a>>),
    BitOr(Box<Expression<'a>>, Box<Expression<'a>>),
    And(Box<Expression<'a>>, Box<Expression<'a>>),
    Or(Box<Expression<'a>>, Box<Expression<'a>>),
    Conditional(
        Box<Expression<'a>>,
        Vec<Statement<'a>>,
        Option<Vec<Statement<'a>>>,
    ),
    Brace(Vec<Statement<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<'a> {
    pub(crate) expr: ExprEnum<'a>,
    pub(crate) span: Span<'a>,
}

impl<'a> Expression<'a> {
    pub(crate) fn new(expr: ExprEnum<'a>, span: Span<'a>) -> Self {
        Self { expr, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnArg<'a> {
    pub name: Option<Span<'a>>,
    pub expr: Expression<'a>,
}

impl<'a> FnArg<'a> {
    #[allow(dead_code)]
    pub(crate) fn new(expr: Expression<'a>) -> Self {
        Self { name: None, expr }
    }
}

/// Calculate offset between the start positions of the input spans and return a span between them.
///
/// Note: `i` shall start earlier than `r`, otherwise wrapping would occur.
fn calc_offset<'a>(i: Span<'a>, r: Span<'a>) -> Span<'a> {
    i.take(i.offset(&r))
}

/// An extension trait for writing subslice concisely
pub(super) trait Subslice {
    fn subslice(&self, start: usize, length: usize) -> Self;
}

impl<'a> Subslice for Span<'a> {
    fn subslice(&self, start: usize, length: usize) -> Self {
        self.take_split(start).0.take(length)
    }
}

fn block_comment<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span, Span, E> {
    let (r, _) = multispace0(input)?;
    delimited(tag("/*"), take_until("*/"), tag("*/"))(r)
}

/// Usually comments are discarded from AST, but in certain places it is preserved for inspection.
fn comment_stmt(input: Span) -> IResult<Span, Statement> {
    if let Ok((r, s)) = block_comment::<nom::error::Error<Span>>(input) {
        return Ok((r, Statement::Comment(s.fragment())));
    }

    match line_comment(input) {
        Ok((r, s)) => Ok((r, Statement::Comment(s.fragment()))),
        Err(e) => Err(e),
    }
}

pub fn identifier(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn ident_space(input: Span) -> IResult<Span, Span> {
    ws(identifier)(input)
}

pub(crate) fn var_ref(input: Span) -> IResult<Span, Expression> {
    let (r, res) = ident_space(input)?;
    Ok((r, Expression::new(ExprEnum::Variable(res.fragment()), res)))
}

fn type_scalar(input: Span) -> IResult<Span, TypeDecl> {
    let (r, type_) = ws(alt((
        tag("f64"),
        tag("f32"),
        tag("i64"),
        tag("i32"),
        tag("str"),
    )))(input)?;
    Ok((
        r,
        match *type_ {
            "f64" => TypeDecl::F64,
            "f32" => TypeDecl::F32,
            "i32" => TypeDecl::I32,
            "i64" => TypeDecl::I64,
            "str" => TypeDecl::Str,
            unknown => {
                unreachable!("Type should have recognized by the parser: \"{}\"", unknown)
            }
        },
    ))
}

fn array_size_range(input: Span) -> IResult<Span, ArraySize> {
    let (r, start) = opt(ws(decimal))(input)?;
    let (r, _) = ws(tag(".."))(r)?;
    let (r, end) = opt(ws(decimal))(r)?;
    let start = start.and_then(|v| v.parse().ok()).unwrap_or(0);
    let end = end.and_then(|v| v.parse().ok()).unwrap_or(usize::MAX);
    Ok((r, ArraySize::Range(start..end)))
}

fn array_size_fixed(input: Span) -> IResult<Span, ArraySize> {
    let (r, v) = ws(decimal)(input)?;
    Ok((
        r,
        ArraySize::Fixed(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn type_array(input: Span) -> IResult<Span, TypeDecl> {
    let (r, (arr, range)) = delimited(
        ws(char('[')),
        pair(
            type_decl,
            opt(preceded(
                tag(";"),
                alt((array_size_range, array_size_fixed)),
            )),
        ),
        ws(char(']')),
    )(input)?;
    Ok((
        r,
        TypeDecl::Array(Box::new(arr), range.unwrap_or_else(|| ArraySize::Any)),
    ))
}

fn type_tuple(i: Span) -> IResult<Span, TypeDecl> {
    let (r, _) = multispace0(i)?;
    let (r, _open_par) = tag("(")(r)?;
    let (r, (mut val, last)) = pair(many0(terminated(type_decl, tag(","))), opt(type_decl))(r)?;
    let (r, _close_par) = tag(")")(r)?;
    if let Some(last) = last {
        val.push(last);
    }
    Ok((r, TypeDecl::Tuple(val)))
}

pub(crate) fn type_decl(input: Span) -> IResult<Span, TypeDecl> {
    alt((type_array, type_tuple, type_scalar))(input)
}

fn cast(i: Span) -> IResult<Span, Expression> {
    let (r, res) = primary_expression(i)?;
    let (r, _) = ws(tag("as"))(r)?;
    let (r, decl) = type_decl(r)?;
    let span = i.subslice(i.offset(&res.span), res.span.offset(&r));
    Ok((
        r,
        Expression::new(ExprEnum::Cast(Box::new(res), decl), span),
    ))
}

pub(crate) fn type_spec(input: Span) -> IResult<Span, TypeDecl> {
    let (r, type_) = opt(delimited(ws(char(':')), type_decl, multispace0))(input)?;
    Ok((r, type_.unwrap_or(TypeDecl::Any)))
}

fn var_decl(input: Span) -> IResult<Span, Statement> {
    let (r, _) = multispace1(tag("var")(multispace0(input)?.0)?.0)?;
    let (r, ident) = ident_space(r)?;
    let (r, ts) = type_spec(r)?;
    let (r, initializer) = opt(preceded(ws(char('=')), full_expression))(r)?;
    let (r, _) = char(';')(ws_comment(r)?.0)?;
    Ok((r, Statement::VarDecl(ident, ts, initializer)))
}

fn decimal(input: Span) -> IResult<Span, Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

fn decimal_value(i: Span) -> IResult<Span, (Value, Span)> {
    let (r, v) = recognize(pair(opt(one_of("+-")), decimal))(i)?;
    let parsed = v.parse().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input: i,
            code: nom::error::ErrorKind::Digit,
        })
    })?;
    Ok((r, (Value::I64(parsed), v)))
}

fn float(input: Span) -> IResult<Span, Span> {
    recognize(tuple((
        opt(one_of("+-")),
        decimal,
        nom::combinator::not(tag("..")),
        char('.'),
        opt(decimal),
    )))(input)
}

fn float_value(i: Span) -> IResult<Span, (Value, Span)> {
    let (r, v) = float(i)?;
    let parsed = v.parse().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input: i,
            code: nom::error::ErrorKind::Digit,
        })
    })?;
    Ok((r, (Value::F64(parsed), v)))
}

fn double_expr(input: Span) -> IResult<Span, Expression> {
    let (r, (value, value_span)) = alt((float_value, decimal_value))(input)?;
    let (r, type_spec) = opt(alt((tag("i32"), tag("i64"), tag("f32"), tag("f64"))))(r)?;
    let (value, ts) = if let Some(ty) = type_spec {
        let map_err = |_e| {
            nom::Err::Error(nom::error::Error::new(
                value_span,
                nom::error::ErrorKind::Digit,
            ))
        };
        match *ty {
            "f64" => (
                Value::F64(coerce_f64(&value).map_err(map_err)?),
                TypeSet::f64(),
            ),
            "f32" => (
                Value::F32(coerce_f32(&value).map_err(map_err)?),
                TypeSet::f32(),
            ),
            "i64" => (
                Value::I64(coerce_i64(&value).map_err(map_err)?),
                TypeSet::i64(),
            ),
            "i32" => (
                Value::I32(coerce_i32(&value).map_err(map_err)?),
                TypeSet::i32(),
            ),
            unknown => {
                unreachable!("Type should have recognized by the parser: \"{}\"", unknown)
            }
        }
    } else {
        let ty = match value {
            Value::I64(_) => TypeSet::int(),
            _ => TypeSet::float(),
        };
        (value, ty)
    };
    Ok((
        r,
        Expression::new(ExprEnum::NumLiteral(value, ts), calc_offset(input, r)),
    ))
}

fn numeric_literal_expression(input: Span) -> IResult<Span, Expression> {
    delimited(multispace0, double_expr, multispace0)(input)
}

fn str_literal(i: Span) -> IResult<Span, Expression> {
    let (r0, _) = multispace0(i)?;
    let (r, _) = preceded(multispace0, char('\"'))(r0)?;
    let (r, val) = many0(none_of("\""))(r)?;
    let (r, _) = terminated(char('"'), multispace0)(r)?;
    Ok((
        r,
        Expression::new(
            ExprEnum::StrLiteral(
                val.iter()
                    .collect::<String>()
                    .replace("\\\\", "\\")
                    .replace("\\n", "\n"),
            ),
            calc_offset(r0, r),
        ),
    ))
}

pub(crate) fn array_literal(i: Span) -> IResult<Span, Expression> {
    let (r, _) = multispace0(i)?;
    let (r, open_br) = tag("[")(r)?;
    let (r, (mut val, last)) = pair(
        many0(terminated(full_expression, tag(","))),
        opt(full_expression),
    )(r)?;
    let (r, close_br) = tag("]")(r)?;
    if let Some(last) = last {
        val.push(last);
    }
    let span = i.subslice(
        i.offset(&open_br),
        open_br.offset(&close_br) + close_br.len(),
    );
    Ok((r, Expression::new(ExprEnum::ArrLiteral(val), span)))
}

pub(crate) fn tuple_literal(i: Span) -> IResult<Span, Expression> {
    let (r, _) = multispace0(i)?;
    let (r, open_br) = tag("(")(r)?;
    let (r, (mut val, last)) = pair(
        many0(terminated(full_expression, tag(","))),
        opt(full_expression),
    )(r)?;
    let (r, close_br) = tag(")")(r)?;
    if let Some(last) = last {
        val.push(last);
    }
    let span = i.subslice(
        i.offset(&open_br),
        open_br.offset(&close_br) + close_br.len(),
    );
    Ok((r, Expression::new(ExprEnum::TupleLiteral(val), span)))
}

// We parse any expr surrounded by parens, ignoring all whitespaces around those
fn parens(i: Span) -> IResult<Span, Expression> {
    let (r0, _) = multispace0(i)?;
    let (r, res) = delimited(tag("("), conditional_expr, tag(")"))(r0)?;
    let (r, _) = multispace0(r)?;
    Ok((r, Expression::new(res.expr, r0.take(r0.offset(&r)))))
}

fn line_comment<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span, Span, E> {
    let (r, _) = multispace0(input)?;
    delimited(tag("//"), take_until("\n"), tag("\n"))(r)
}

fn ws_comment<'a, E: ParseError<Span<'a>>>(i: Span<'a>) -> IResult<Span, (), E> {
    let (r, _) = many0(alt((line_comment, block_comment, multispace1)))(i)?;

    Ok((r, ()))
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: ParseError<Span<'a>>>(
    inner: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, E>
where
    F: FnMut(Span<'a>) -> IResult<Span<'a>, O, E>,
{
    delimited(ws_comment, inner, ws_comment)
}

pub(crate) fn fn_invoke_arg(i: Span) -> IResult<Span, FnArg> {
    let (r, name) = opt(pair(ws(identifier), ws(tag(":"))))(i)?;
    let (r, expr) = full_expression(r)?;
    Ok((
        r,
        FnArg {
            name: name.map(|(a, _)| a),
            expr,
        },
    ))
}

pub(crate) fn func_invoke(i: Span) -> IResult<Span, Expression> {
    let (r, ident) = ws(identifier)(i)?;
    // println!("func_invoke ident: {}", ident);
    let (r, args) = delimited(
        multispace0,
        delimited(
            char('('),
            terminated(
                separated_list0(ws(char(',')), fn_invoke_arg),
                opt(ws(char(','))),
            ),
            char(')'),
        ),
        multispace0,
    )(r)?;
    Ok((
        r,
        Expression::new(
            ExprEnum::FnInvoke(*ident, args),
            i.subslice(i.offset(&ident), ident.offset(&r)),
        ),
    ))
}

pub(crate) fn array_index(i: Span) -> IResult<Span, Expression> {
    let (r, prim) = primary_expression(i)?;
    let (r, indices) = many1(delimited(
        multispace0,
        delimited(
            tag("["),
            separated_list1(ws(char(',')), full_expression),
            tag("]"),
        ),
        multispace0,
    ))(r)?;
    let prim_span = prim.span;
    Ok((
        r,
        indices.into_iter().fold(prim, |acc, v| {
            Expression::new(
                ExprEnum::ArrIndex(Box::new(acc), v),
                i.subslice(i.offset(&prim_span), prim_span.offset(&r)),
            )
        }),
    ))
}

pub(crate) fn tuple_index(i: Span) -> IResult<Span, Expression> {
    let (r, prim) = primary_expression(i)?;
    let (r, indices) = many1(ws(preceded(tag("."), digit1)))(r)?;
    let prim_span = prim.span;
    Ok((
        r,
        indices
            .into_iter()
            .fold(Ok(prim), |acc, v: Span| -> Result<_, _> {
                Ok(Expression::new(
                    ExprEnum::TupleIndex(
                        Box::new(acc?),
                        v.parse().map_err(|_| {
                            nom::Err::Error(nom::error::Error {
                                input: i,
                                code: nom::error::ErrorKind::Digit,
                            })
                        })?,
                    ),
                    i.subslice(i.offset(&prim_span), prim_span.offset(&r)),
                ))
            })?,
    ))
}

pub(crate) fn primary_expression(i: Span) -> IResult<Span, Expression> {
    alt((
        numeric_literal_expression,
        str_literal,
        array_literal,
        var_ref,
        parens,
        brace_expr,
        tuple_literal,
    ))(i)
}

fn postfix_expression(i: Span) -> IResult<Span, Expression> {
    alt((
        func_invoke,
        array_index,
        tuple_index,
        cast,
        primary_expression,
    ))(i)
}

fn not(i: Span) -> IResult<Span, Expression> {
    let (r, op) = delimited(
        multispace0,
        alt((char('!'), char('~'), char('-'))),
        multispace0,
    )(i)?;
    let (r, v) = not_factor(r)?;
    Ok((
        r,
        Expression::new(
            match op {
                '!' => ExprEnum::Not(Box::new(v)),
                '~' => ExprEnum::BitNot(Box::new(v)),
                '-' => ExprEnum::Neg(Box::new(v)),
                _ => unreachable!("not operator should be ! or ~"),
            },
            calc_offset(i, r),
        ),
    ))
}

fn not_factor(i: Span) -> IResult<Span, Expression> {
    alt((not, postfix_expression))(i)
}

// We read an initial factor and for each time we find
// a * or / operator followed by another factor, we do
// the math by folding everything
fn term(i: Span) -> IResult<Span, Expression> {
    let (r, init) = not_factor(i)?;

    fold_many0(
        pair(alt((char('*'), char('/'))), not_factor),
        move || init.clone(),
        move |acc, (op, val): (char, Expression)| {
            let span = i.subslice(
                i.offset(&acc.span),
                acc.span.offset(&val.span) + val.span.len(),
            );
            if op == '*' {
                Expression::new(ExprEnum::Mult(Box::new(acc), Box::new(val)), span)
            } else {
                Expression::new(ExprEnum::Div(Box::new(acc), Box::new(val)), span)
            }
        },
    )(r)
}

pub(crate) fn expr(i: Span) -> IResult<Span, Expression> {
    let (r, init) = term(i)?;

    fold_many0(
        pair(alt((char('+'), char('-'))), term),
        move || init.clone(),
        move |acc, (op, val): (char, Expression)| {
            let span = i.subslice(
                i.offset(&acc.span),
                acc.span.offset(&val.span) + val.span.len(),
            );
            if op == '+' {
                Expression::new(ExprEnum::Add(Box::new(acc), Box::new(val)), span)
            } else {
                Expression::new(ExprEnum::Sub(Box::new(acc), Box::new(val)), span)
            }
        },
    )(r)
}

fn cmp(i: Span) -> IResult<Span, Expression> {
    let (r, lhs) = expr(i)?;

    let (r, (op, val)) = pair(alt((char('<'), char('>'))), expr)(r)?;
    let span = calc_offset(i, r);
    Ok((
        r,
        if op == '<' {
            Expression::new(ExprEnum::LT(Box::new(lhs), Box::new(val)), span)
        } else {
            Expression::new(ExprEnum::GT(Box::new(lhs), Box::new(val)), span)
        },
    ))
}

pub(crate) fn conditional(i: Span) -> IResult<Span, Expression> {
    let (r, _) = ws(tag("if"))(i)?;
    let (r, cond) = or(r)?;
    let (r, true_branch) = delimited(ws(char('{')), source, ws(char('}')))(r)?;
    let (r, false_branch) = opt(preceded(
        ws(tag("else")),
        alt((
            delimited(ws(char('{')), source, ws(char('}'))),
            map_res(
                conditional,
                |ex| -> Result<Vec<Statement>, nom::error::Error<&str>> {
                    Ok(vec![Statement::Expression {
                        ex,
                        semicolon: false,
                    }])
                },
            ),
        )),
    ))(r)?;
    Ok((
        r,
        Expression::new(
            ExprEnum::Conditional(Box::new(cond), true_branch, false_branch),
            calc_offset(i, r),
        ),
    ))
}

pub(crate) fn var_assign(i: Span) -> IResult<Span, Expression> {
    let (r, res) = tuple((cmp_expr, char('='), assign_expr))(i)?;
    let span = calc_offset(i, r);
    Ok((
        r,
        Expression::new(ExprEnum::VarAssign(Box::new(res.0), Box::new(res.2)), span),
    ))
}

pub(crate) fn cmp_expr(i: Span) -> IResult<Span, Expression> {
    alt((cmp, expr))(i)
}

/// A functor to create a function for a binary operator
fn bin_op<'src>(
    t: &'static str,
    sub: impl Fn(Span<'src>) -> IResult<Span<'src>, Expression<'src>>,
    cons: impl Fn(Box<Expression<'src>>, Box<Expression<'src>>) -> ExprEnum<'src>,
) -> impl Fn(Span<'src>) -> IResult<Span<'src>, Expression<'src>> {
    move |i| {
        let sub = &sub;
        let cons = &cons;
        let (r, init) = sub(i)?;

        fold_many0(
            pair(ws(tag(t)), sub),
            move || init.clone(),
            move |acc: Expression, (_, val): (Span, Expression)| {
                let span = i.subslice(
                    i.offset(&acc.span),
                    acc.span.offset(&val.span) + val.span.len(),
                );
                Expression::new(cons(Box::new(acc), Box::new(val)), span)
            },
        )(r)
    }
}

fn bit_and(i: Span) -> IResult<Span, Expression> {
    bin_op("&", cmp_expr, |lhs, rhs| ExprEnum::BitAnd(lhs, rhs))(i)
}

fn bit_xor(i: Span) -> IResult<Span, Expression> {
    bin_op("^", bit_and, |lhs, rhs| ExprEnum::BitXor(lhs, rhs))(i)
}

fn bit_or(i: Span) -> IResult<Span, Expression> {
    bin_op("|", bit_xor, |lhs, rhs| ExprEnum::BitOr(lhs, rhs))(i)
}

fn and(i: Span) -> IResult<Span, Expression> {
    bin_op("&&", bit_or, |lhs, rhs| ExprEnum::And(lhs, rhs))(i)
}

fn or(i: Span) -> IResult<Span, Expression> {
    bin_op("||", and, |lhs, rhs| ExprEnum::Or(lhs, rhs))(i)
}

fn assign_expr(i: Span) -> IResult<Span, Expression> {
    alt((var_assign, or))(i)
}

pub(crate) fn conditional_expr(i: Span) -> IResult<Span, Expression> {
    alt((conditional, assign_expr))(i)
}

fn brace_expr(i: Span) -> IResult<Span, Expression> {
    let (r, open_br) = ws(tag("{"))(i)?;
    let (r, v) = source(r)?;
    let (r, close_br) = ws(tag("}"))(r)?;
    let span = i.subslice(
        i.offset(&open_br),
        open_br.offset(&close_br) + close_br.len(),
    );
    Ok((r, Expression::new(ExprEnum::Brace(v), span)))
}

pub(crate) fn full_expression(input: Span) -> IResult<Span, Expression> {
    conditional_expr(input)
}

fn expression_statement(input: Span) -> IResult<Span, Statement> {
    let (r, val) = full_expression(input)?;
    Ok((
        r,
        Statement::Expression {
            ex: val,
            semicolon: false,
        },
    ))
}

pub(crate) fn func_arg(r: Span) -> IResult<Span, ArgDecl> {
    let (r, id) = identifier(r)?;
    let (r, ty) = opt(ws(type_spec))(r)?;
    let (r, init) = opt(preceded(ws(char('=')), full_expression))(r)?;
    Ok((
        r,
        ArgDecl {
            name: id,
            ty: ty.unwrap_or(TypeDecl::F64),
            init,
        },
    ))
}

fn type_set(input: Span) -> IResult<Span, TypeSet> {
    type_decl(input).map_or_else(
        |_| {
            let (r, _) = tag("void")(input)?;
            Ok((r, TypeSet::void()))
        },
        |(r, ty)| Ok((r, ty.into())),
    )
}

pub(crate) fn func_decl(input: Span) -> IResult<Span, Statement> {
    let (r, _) = ws(tag("fn"))(input)?;
    let (r, name) = identifier(r)?;
    let (r, args) = ws(delimited(
        tag("("),
        terminated(separated_list0(ws(tag(",")), func_arg), opt(ws(char(',')))),
        tag(")"),
    ))(r)?;
    let (r, ret_type) = opt(preceded(ws(tag("->")), type_set))(r)?;
    let (r, stmts) = delimited(ws(char('{')), source, ws(char('}')))(r)?;
    Ok((
        r,
        Statement::FnDecl {
            name,
            args,
            ret_type: ret_type
                .map(|t| t.into())
                .unwrap_or_else(|| TypeSet::void()),
            stmts: Rc::new(stmts),
        },
    ))
}

fn loop_stmt(input: Span) -> IResult<Span, Statement> {
    let (r, _) = ws(tag("loop"))(input)?;
    let (r, stmts) = delimited(ws(char('{')), source, ws(char('}')))(r)?;
    Ok((r, Statement::Loop(stmts)))
}

fn while_stmt(input: Span) -> IResult<Span, Statement> {
    let (r, _) = ws(tag("while"))(input)?;
    let (r, cond) = cmp_expr(r)?;
    let (r, stmts) = delimited(ws(char('{')), source, ws(char('}')))(r)?;
    Ok((r, Statement::While(cond, stmts)))
}

fn for_stmt(input: Span) -> IResult<Span, Statement> {
    let (r, _) = ws(tag("for"))(input)?;
    let (r, iter) = identifier(r)?;
    let (r, _) = ws(tag("in"))(r)?;
    let (r, from) = expr(r)?;
    let (r, _) = ws(tag(".."))(r)?;
    let (r, to) = expr(r)?;
    let (r, stmts) = delimited(ws(char('{')), source, ws(char('}')))(r)?;
    Ok((r, Statement::For(iter, from, to, stmts)))
}

fn break_stmt(input: Span) -> IResult<Span, Statement> {
    let (r, _) = ws(tag("break"))(input)?;
    Ok((r, Statement::Break))
}

pub(crate) fn statement(input: Span) -> IResult<Span, Statement> {
    alt((
        var_decl,
        func_decl,
        loop_stmt,
        while_stmt,
        for_stmt,
        break_stmt,
        expression_statement,
        comment_stmt,
    ))(input)
}

pub fn source(mut input: Span) -> IResult<Span, Vec<Statement>> {
    // This ugly loop with pushing to the vec is necessary to cary over parsed state (stmt.expects_semicolon())
    // which can affect the next statement's syntax.
    let mut v = vec![];
    loop {
        let (mut r, mut stmt) = match statement(input) {
            Ok((r, stmt)) => (r, stmt),
            Err(e) => {
                if matches!(e, nom::Err::Failure(_)) {
                    return Err(e);
                } else {
                    return Ok((ws_comment(input)?.0, v));
                }
            }
        };
        if stmt.expects_semicolon() {
            let (rr, semicolon) = opt(ws(tag(";")))(r)?;
            if semicolon.is_some() {
                if let Statement::Expression {
                    ref mut semicolon, ..
                } = stmt
                {
                    *semicolon = true;
                }
            }
            r = rr;
        }
        v.push(stmt);
        input = r;
    }
}

pub fn span_source(input: &str) -> IResult<Span, Vec<Statement>> {
    source(Span::new(input))
}

#[cfg(test)]
pub(crate) mod test;
