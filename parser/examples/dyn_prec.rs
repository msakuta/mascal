//! A demonstration of precedence climbing method
//!
//! It is a more stack-efficient way to parse binary operator expressions.
//! It is also eaiser to adapt to custom operators with different precedence
//! and associativity at runtime.

use std::convert::TryFrom;

fn main() {
    let args = std::env::args();
    for arg in args {
        if arg == "-d" {
            DEBUG.store(true, std::sync::atomic::Ordering::Relaxed);
        }
    }
    test_case("123");
    test_case("Hello + world");
    test_case("1 * 3");
    test_case("1 + 2 + 3");
    test_case("1 - 2 + 3");
    test_case("10 + 1 * 3");
    test_case("10 * 1 + 3");
    test_case("10 + 1 * 3 + 100");
    test_case("9 / 3 / 3");
    test_case("(123 + 456 ) + world");
    test_case("car + cdr + cdr");
    test_case("((1 + 2) + (3 + 4)) + 5 + 6 * 7");
    test_case("5 ^ 6 ^ 7");
}

fn test_case(input: &str) {
    let op_defs = standard_ops();
    let lexer = Lexer::new(&op_defs, input);
    match expr(lexer) {
        Some((_, res)) => {
            println!("source: {:?}, parsed: {}", input, res);
        }
        _ => {
            println!("source: {input:?}, failed");
        }
    }
}

fn standard_ops() -> Vec<OpDef> {
    vec![
        OpDef::new("+", 1, Associativity::Left),
        OpDef::new("-", 1, Associativity::Left),
        OpDef::new("*", 2, Associativity::Left),
        OpDef::new("/", 2, Associativity::Left),
        OpDef::new("^", 3, Associativity::Right),
    ]
}

fn advance_char(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
    input.chars().next()
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct OpDef {
    code: String,
    prec: usize,
    assoc: Associativity,
}

impl std::fmt::Display for OpDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.code)
    }
}

impl OpDef {
    fn new(code: impl Into<String>, prec: usize, assoc: Associativity) -> Self {
        Self {
            code: code.into(),
            prec,
            assoc,
        }
    }
}

type OpDefs = Vec<OpDef>;

#[derive(Debug, PartialEq, Clone)]
enum Token<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Op(OpDef),
    LParen,
    RParen,
}

#[derive(Debug, PartialEq)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    BinOp {
        op: OpDef,
        lhs: Box<Expression<'src>>,
        rhs: Box<Expression<'src>>,
    },
}

impl<'src> TryFrom<Token<'src>> for Expression<'src> {
    type Error = ();
    fn try_from(value: Token<'src>) -> Result<Self, Self::Error> {
        match value {
            Token::Ident(id) => Ok(Expression::Ident(id)),
            Token::NumLiteral(num) => Ok(Expression::NumLiteral(num)),
            _ => Err(()),
        }
    }
}

impl<'src> std::fmt::Display for Expression<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(id) => write!(f, "{id}"),
            Self::NumLiteral(num) => write!(f, "{num}"),
            Self::BinOp { op, lhs, rhs } => {
                write!(f, "(")?;
                lhs.fmt(f)?;
                write!(f, " {op} ")?;
                rhs.fmt(f)?;
                write!(f, ")")
            }
        }
    }
}

impl<'src> Expression<'src> {
    fn bin_op(op: OpDef, lhs: Self, rhs: Self) -> Self {
        Self::BinOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

fn precedence(op: &OpDef) -> usize {
    op.prec
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Associativity {
    Left,
    Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Lexer<'src> {
    op_defs: &'src OpDefs,
    cur: &'src str,
}

impl<'src> Lexer<'src> {
    fn new(op_defs: &'src OpDefs, input: &'src str) -> Self {
        Self {
            op_defs,
            cur: input,
        }
    }

    fn next(&mut self) -> Option<Token<'src>> {
        if let Some((r, res)) = token(self.op_defs)(self.cur) {
            self.cur = r;
            return Some(res);
        }
        None
    }

    fn peek(&self) -> Option<Token<'src>> {
        if let Some((_, res)) = token(self.op_defs)(self.cur) {
            return Some(res);
        }
        None
    }
}

fn expr(lexer: Lexer) -> Option<(Lexer, Expression)> {
    if let Some(res) = bin_op(0)(lexer) {
        dprintln!("bin_op returned {:?}", res.0);
        return Some(res);
    }

    None
}

fn paren(mut lexer: Lexer) -> Option<(Lexer, Expression)> {
    let Some(Token::LParen) = lexer.next() else {
        return None;
    };

    let (next_lexer, expr) = expr(lexer)?;
    lexer = next_lexer;

    dprintln!("paren got expr {expr}, lexer: {lexer:?}");

    let Some(Token::RParen) = lexer.next() else {
        return None;
    };

    Some((lexer, expr))
}

static DEBUG: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

#[macro_export]
macro_rules! dprintln {
    ($fmt:literal) => {
        if DEBUG.load(std::sync::atomic::Ordering::Relaxed) {
            println!($fmt);
        }
    };
    ($fmt:literal, $($args:expr),*) => {
        if DEBUG.load(std::sync::atomic::Ordering::Relaxed) {
            println!($fmt, $($args),*);
        }
    };
}
fn bin_op<'src>(
    prec: usize,
) -> impl Fn(Lexer<'src>) -> Option<(Lexer<'src>, Expression<'src>)> + 'src {
    move |mut lexer: Lexer| {
        let (next, mut ret) = term(lexer)?;
        lexer = next;
        dprintln!("[{prec}] First token: {ret:?}");
        dprintln!("[{prec}] First expression: {ret:?} next: {lexer:?}");
        let Some(mut lookahead) = lexer.peek() else {
            return Some((lexer, ret));
        };
        dprintln!("[{prec}] First op: {lookahead:?}");
        while let Token::Op(op) = lookahead.clone() {
            if precedence(&op) < prec {
                break;
            }
            lexer.next()?;
            let inner_next = lexer;
            let (outer_next, mut rhs) = term(lexer)?;
            lexer = outer_next;
            dprintln!("[{prec}] Outer loop Next token: {rhs:?}");
            let Some(p_lookahead) = lexer.peek() else {
                dprintln!("[{prec}] Exhausted input, returning {ret:?} and {rhs:?}");
                return Some((lexer, Expression::bin_op(op, ret, rhs)));
            };
            dprintln!("[{prec}] Outer lookahead: {p_lookahead:?} next: {lexer:?}");
            // (outer_next, lookahead) = (rhs_next, p_lookahead);
            lookahead = p_lookahead;

            while let Token::Op(next_op) = lookahead.clone() {
                if precedence(&next_op) <= precedence(&op)
                    && (precedence(&next_op) != precedence(&op) || op.assoc != Associativity::Right)
                {
                    break;
                }
                let next_prec = precedence(&op)
                    + if precedence(&op) < precedence(&next_op) {
                        1
                    } else {
                        0
                    };
                dprintln!(
                    "[{prec}] Inner next_prec: {:?} , inner_next: {inner_next:?}",
                    next_prec
                );
                (lexer, rhs) = bin_op(next_prec)(inner_next)?;
                let Some(p_lookahead) = lexer.peek() else {
                    dprintln!("[{prec}] Inner Exhausted input, returning {ret:?} and {rhs:?}");
                    return Some((lexer, Expression::bin_op(op, ret, rhs)));
                };
                lookahead = p_lookahead;
            }
            dprintln!("[{prec}] Combining bin_op outer: {ret:?}, {rhs:?}, next: {lexer:?} lookahead: {lookahead:?}");
            ret = Expression::bin_op(op, ret, rhs);
        }

        dprintln!("[{prec}] Exiting normally with {ret:?}");

        Some((lexer, ret))
    }
}

fn term<'src>(mut lexer: Lexer<'src>) -> Option<(Lexer<'src>, Expression<'src>)> {
    if let Some(res) = paren(lexer) {
        return Some(res);
    }

    if let Some(res) = lexer.next() {
        let ex = match res {
            Token::Ident(id) => Expression::Ident(id),
            Token::NumLiteral(num) => Expression::NumLiteral(num),
            _ => return None,
        };
        return Some((lexer, ex));
    }

    None
}

fn token(op_defs: &OpDefs) -> impl Fn(&str) -> Option<(&str, Token)> + '_ {
    move |input: &str| {
        if let Some(r) = lparen(whitespace(input)) {
            return Some((r, Token::LParen));
        }
        if let Some(r) = rparen(whitespace(input)) {
            return Some((r, Token::RParen));
        }
        if let Some(res) = ident(whitespace(input)) {
            return Some(res);
        }
        if let Some(res) = number(whitespace(input)) {
            return Some(res);
        }
        if let Some(res) = operator(op_defs)(whitespace(input)) {
            return Some(res);
        }
        None
    }
}

fn whitespace(mut input: &str) -> &str {
    while matches!(peek_char(input), Some(' ')) {
        input = advance_char(input);
    }
    input
}

fn ident(mut input: &str) -> Option<(&str, Token<'_>)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        input = advance_char(input);
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input);
        }
    }
    if start.len() == input.len() {
        None
    } else {
        Some((input, Token::Ident(&start[..(start.len() - input.len())])))
    }
}

fn number(mut input: &str) -> Option<(&str, Token<'_>)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '.' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
    }
    if let Ok(num) = start[..(start.len() - input.len())].parse::<f64>() {
        Some((input, Token::NumLiteral(num)))
    } else {
        None
    }
}

fn operator(op_defs: &OpDefs) -> impl Fn(&str) -> Option<(&str, Token)> + '_ {
    move |input: &str| {
        for op_def in op_defs {
            if op_def.code.len() <= input.len() && input[..op_def.code.len()] == op_def.code {
                return Some((&input[op_def.code.len()..], Token::Op(op_def.clone())));
            }
        }
        None
    }
}

fn lparen(input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('(')) {
        Some(advance_char(input))
    } else {
        None
    }
}

fn rparen(input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some(')')) {
        Some(advance_char(input))
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("    "), "");
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("Adam"), Some(("", Token::Ident("Adam"))));
    }

    #[test]
    fn test_number() {
        assert_eq!(number("123.45 "), Some((" ", Token::NumLiteral(123.45))));
    }

    use Expression::NumLiteral as Lit;
    use OpDef::*;

    fn e_bin_op<'src>(op: OpDef, lhs: Expression<'src>, rhs: Expression<'src>) -> Expression<'src> {
        Expression::bin_op(op, lhs, rhs)
    }

    #[test]
    fn test_expr() {
        let op_defs = standard_ops();
        assert_eq!(
            expr(Lexer::new(&op_defs, "1 + 2 * 3")),
            Some((
                Lexer::new(&op_defs, ""),
                e_bin_op(Add, Lit(1.), e_bin_op(Mul, Lit(2.), Lit(3.)))
            ))
        );
    }

    #[test]
    fn test_associativity() {
        let op_defs = standard_ops();
        assert_eq!(
            expr(Lexer::new(&op_defs, "1 - 2 + 3 ")),
            Some((
                Lexer::new(&op_defs, " "),
                e_bin_op(Add, e_bin_op(Sub, Lit(1.), Lit(2.)), Lit(3.),)
            ))
        );
    }
}
