#![cfg(test)]

use super::*;
use nom::Finish;
use ExprEnum::*;

#[test]
fn test_comments() {
    let res = comment_stmt(Span::new("/* x * y */")).unwrap();
    assert_eq!(res.0.fragment(), &"");
    assert_eq!(res.1, Statement::Comment(" x * y "));
}

#[test]
fn test_comments_error() {
    if let Err(e) = comment_stmt(Span::new("/* x * y")).finish() {
        assert_eq!(e.input.fragment(), &"/* x * y");
        assert_eq!(e.code, nom::error::ErrorKind::Tag);
    } else {
        panic!();
    };
}

#[test]
fn test_line_comment() {
    let span = Span::new(" // hey  \n");
    assert_eq!(
        line_comment::<nom::error::Error<Span>>(span)
            .finish()
            .unwrap()
            .1,
        span.subslice(3, 6)
    );
}

#[test]
fn test_block_comment() {
    let span = Span::new(" /* hey  */\n");
    assert_eq!(
        block_comment::<nom::error::Error<Span>>(span)
            .finish()
            .unwrap()
            .1,
        span.subslice(3, 6)
    );
}

#[test]
fn test_comment() {
    let span = Span::new("/* hey  */");
    assert_eq!(
        ws_comment::<nom::error::Error<Span>>(span).finish(),
        Ok((span.subslice(span.len(), 0), ()))
    );
}

#[test]
fn test_ident() {
    let res = ident_space(Span::new("x123  ")).unwrap();
    assert_eq!(res.0.fragment(), &"");
    assert_eq!(res.1.fragment(), &"x123");
}

#[test]
fn test_add() {
    let span = Span::new("123.4 + 456");
    let res = expression_statement(span).finish().unwrap().1;
    assert_eq!(
        res,
        Statement::Expression(Expression::new(
            Add(
                Box::new(Expression::new(NumLiteral(Value::F64(123.4)), span.take(5))),
                Box::new(Expression::new(
                    NumLiteral(Value::I64(456)),
                    span.take_split(8).0
                ))
            ),
            span
        ))
    );
}

#[test]
fn test_add_vars() {
    use nom::Finish;

    let span = Span::new("a + b");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(
            Add(
                Box::new(Expression::new(Variable("a"), span.take(1))),
                Box::new(Expression::new(Variable("b"), span.subslice(4, 1)))
            ),
            span,
        )
    );
}

#[test]
fn test_add_paren() {
    let span = Span::new("123.4 + (456 + 789.5)");
    let res = expression_statement(span).finish().unwrap().1;
    assert_eq!(
        res,
        Statement::Expression(Expression::new(
            Add(
                Box::new(Expression::new(NumLiteral(Value::F64(123.4)), span.take(5))),
                Box::new(Expression::new(
                    Add(
                        Box::new(Expression::new(
                            NumLiteral(Value::I64(456)),
                            span.subslice(9, 3)
                        )),
                        Box::new(Expression::new(
                            NumLiteral(Value::F64(789.5)),
                            span.subslice(15, 5)
                        )),
                    ),
                    span.subslice(8, 13)
                ))
            ),
            span
        ))
    );
}

#[test]
fn str_test() {
    let span = Span::new("\"hello\"");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(StrLiteral("hello".to_string()), span)
    );
    let span = Span::new("\"sl\\\\ash\"");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(StrLiteral("sl\\ash".to_string()), span)
    );
    let span = Span::new("\"new\\nline\"");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(StrLiteral("new\nline".to_string()), span)
    );
}

#[test]
fn expr_test() {
    let span = Span::new(" 1 +  2 ");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(
            Add(
                Box::new(Expression::new(
                    NumLiteral(Value::I64(1)),
                    span.subslice(1, 1)
                )),
                Box::new(Expression::new(
                    NumLiteral(Value::I64(2)),
                    span.subslice(6, 1)
                ))
            ),
            span.subslice(1, 6)
        )
    );
    let span = Span::new(" 12 + 6 - 4+  3");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(
            Add(
                Box::new(Expression::new(
                    Sub(
                        Box::new(Expression::new(
                            Add(
                                Box::new(Expression::new(
                                    NumLiteral(Value::I64(12)),
                                    span.subslice(1, 2)
                                )),
                                Box::new(Expression::new(
                                    NumLiteral(Value::I64(6)),
                                    span.subslice(6, 1)
                                ))
                            ),
                            span.subslice(1, 6)
                        )),
                        Box::new(Expression::new(
                            NumLiteral(Value::I64(4)),
                            span.subslice(10, 1)
                        )),
                    ),
                    span.subslice(1, 10)
                )),
                Box::new(Expression::new(
                    NumLiteral(Value::I64(3)),
                    span.subslice(14, 1)
                ))
            ),
            span.subslice(1, 14)
        )
    );
    let span = Span::new(" 1 + 2*3 + 4");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(
            Add(
                Box::new(Expression::new(
                    Add(
                        Box::new(Expression::new(
                            NumLiteral(Value::I64(1)),
                            span.subslice(1, 1)
                        )),
                        Box::new(Expression::new(
                            Mult(
                                Box::new(Expression::new(
                                    NumLiteral(Value::I64(2)),
                                    span.subslice(5, 1)
                                )),
                                Box::new(Expression::new(
                                    NumLiteral(Value::I64(3)),
                                    span.subslice(7, 1)
                                ))
                            ),
                            span.subslice(5, 3)
                        ))
                    ),
                    span.subslice(1, 7)
                )),
                Box::new(Expression::new(
                    NumLiteral(Value::I64(4)),
                    span.subslice(11, 1)
                ))
            ),
            span.subslice(1, 11)
        )
    );
}

#[test]
fn parens_test() {
    let span = Span::new(" (  2 )");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(NumLiteral(Value::I64(2)), span.subslice(1, 6))
    );
    let span = Span::new(" 2* (  3 + 4 ) ");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(
            Mult(
                Box::new(Expression::new(
                    NumLiteral(Value::I64(2)),
                    span.subslice(1, 1)
                )),
                Box::new(Expression::new(
                    Add(
                        Box::new(Expression::new(
                            NumLiteral(Value::I64(3)),
                            span.subslice(7, 1)
                        )),
                        Box::new(Expression::new(
                            NumLiteral(Value::I64(4)),
                            span.subslice(11, 1)
                        )),
                    ),
                    span.subslice(4, 11)
                ))
            ),
            span.subslice(1, 14)
        )
    );
    let span = Span::new("  2*2 / ( 5 - 1) + 3");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(
            Add(
                Box::new(Expression::new(
                    Div(
                        Box::new(Expression::new(
                            Mult(
                                Box::new(Expression::new(
                                    NumLiteral(Value::I64(2)),
                                    span.subslice(2, 1)
                                )),
                                Box::new(Expression::new(
                                    NumLiteral(Value::I64(2)),
                                    span.subslice(4, 1)
                                )),
                            ),
                            span.subslice(2, 3)
                        )),
                        Box::new(Expression::new(
                            Sub(
                                Box::new(Expression::new(
                                    NumLiteral(Value::I64(5)),
                                    span.subslice(10, 1)
                                )),
                                Box::new(Expression::new(
                                    NumLiteral(Value::I64(1)),
                                    span.subslice(14, 1)
                                )),
                            ),
                            span.subslice(8, 9)
                        )),
                    ),
                    span.subslice(2, 15)
                )),
                Box::new(Expression::new(
                    NumLiteral(Value::I64(3)),
                    span.subslice(19, 1)
                )),
            ),
            span.subslice(2, 18)
        )
    );
}

fn var_r(name: Span) -> Box<Expression> {
    Box::new(Expression::new(ExprEnum::Variable(*name), name))
}

#[test]
fn fn_decl_test() {
    let span = Span::new(
        "fn f(a) {
    x = 123;
    x * a;
}",
    );
    assert_eq!(
        func_decl(span).finish().unwrap().1,
        Statement::FnDecl {
            name: "f",
            args: vec![ArgDecl::new("a", TypeDecl::Any)],
            ret_type: None,
            stmts: Rc::new(vec![
                Statement::Expression(Expression::new(
                    VarAssign(
                        var_r(span.subslice(14, 1)),
                        Box::new(Expression::new(
                            NumLiteral(Value::I64(123)),
                            span.subslice(18, 3)
                        ))
                    ),
                    span.subslice(14, 7)
                )),
                Statement::Expression(Expression::new(
                    Mult(var_r(span.subslice(27, 1)), var_r(span.subslice(31, 1))),
                    span.subslice(27, 5)
                ))
            ])
        }
    );
    assert_eq!(
        func_arg(Span::new("a: i32")).finish().unwrap().1,
        ArgDecl::new("a", TypeDecl::I32)
    );
    let span = Span::new("fn f(a: i32) { a * 2 }");
    assert_eq!(
        func_decl(span).finish().unwrap().1,
        Statement::FnDecl {
            name: "f",
            args: vec![ArgDecl::new("a", TypeDecl::I32)],
            ret_type: None,
            stmts: Rc::new(vec![Statement::Expression(Expression::new(
                Mult(
                    var_r(span.subslice(15, 1)),
                    Box::new(Expression::new(
                        NumLiteral(Value::I64(2)),
                        span.subslice(19, 1)
                    ))
                ),
                span.subslice(15, 5)
            ))])
        }
    );
    let span = Span::new("fn f(a: i32) -> f64 { a * 2 }");
    assert_eq!(
        func_decl(span).finish().unwrap().1,
        Statement::FnDecl {
            name: "f",
            args: vec![ArgDecl::new("a", TypeDecl::I32)],
            ret_type: Some(TypeDecl::F64),
            stmts: Rc::new(vec![Statement::Expression(Expression::new(
                Mult(
                    var_r(span.subslice(22, 1)),
                    Box::new(Expression::new(
                        NumLiteral(Value::I64(2)),
                        span.subslice(26, 1)
                    ))
                ),
                span.subslice(22, 5)
            ))])
        }
    );
}

#[test]
fn test_variable() {
    let span = Span::new("b");
    assert_eq!(
        expr(span).finish().unwrap().1,
        Expression::new(Variable("b"), span)
    );
}

#[test]
fn test_cmp_vars() {
    let span = Span::new("a < b");
    assert_eq!(
        cmp(span).finish().unwrap().1,
        Expression::new(
            LT(
                Box::new(Expression::new(Variable("a"), span.take(1))),
                Box::new(Expression::new(Variable("b"), span.subslice(4, 1)))
            ),
            span,
        )
    );
}
#[test]
fn test_cmp_literal() {
    let span = Span::new("a < 100");
    assert_eq!(
        cmp(span).finish().unwrap().1,
        Expression::new(
            LT(
                Box::new(Expression::new(Variable("a"), span.take(1))),
                Box::new(Expression::new(
                    NumLiteral(Value::I64(100)),
                    span.subslice(4, 3)
                ))
            ),
            span,
        )
    );
}

#[test]
fn test_bit_or() {
    use nom::Finish;
    let span = Span::new("1 | 2");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ExprEnum::BitOr(
                Box::new(Expression::new(
                    ExprEnum::NumLiteral(Value::I64(1)),
                    span.subslice(0, 1)
                )),
                Box::new(Expression::new(
                    ExprEnum::NumLiteral(Value::I64(2)),
                    span.subslice(4, 1)
                ))
            ),
            span
        )
    );
}

#[test]
fn test_bit_not() {
    use nom::Finish;
    let span = Span::new("~1");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ExprEnum::BitNot(Box::new(Expression::new(
                ExprEnum::NumLiteral(Value::I64(1)),
                span.subslice(1, 1)
            ))),
            span
        )
    );
}

#[test]
fn test_bit_or_var() {
    use nom::Finish;
    let span = Span::new("1 | 2 && 3");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ExprEnum::And(
                Box::new(Expression::new(
                    ExprEnum::BitOr(
                        Box::new(Expression::new(
                            ExprEnum::NumLiteral(Value::I64(1)),
                            span.subslice(0, 1)
                        )),
                        Box::new(Expression::new(
                            ExprEnum::NumLiteral(Value::I64(2)),
                            span.subslice(4, 1)
                        ))
                    ),
                    span.subslice(0, 5)
                )),
                Box::new(Expression::new(
                    ExprEnum::NumLiteral(Value::I64(3)),
                    span.subslice(9, 1)
                ))
            ),
            span
        )
    );
}

#[test]
fn test_bit_and_var() {
    use nom::Finish;
    let span = Span::new("1 & 2 && 3");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ExprEnum::And(
                Box::new(Expression::new(
                    ExprEnum::BitAnd(
                        Box::new(Expression::new(
                            ExprEnum::NumLiteral(Value::I64(1)),
                            span.subslice(0, 1)
                        )),
                        Box::new(Expression::new(
                            ExprEnum::NumLiteral(Value::I64(2)),
                            span.subslice(4, 1)
                        ))
                    ),
                    span.subslice(0, 5)
                )),
                Box::new(Expression::new(
                    ExprEnum::NumLiteral(Value::I64(3)),
                    span.subslice(9, 1)
                ))
            ),
            span
        )
    );
}

#[test]
fn test_bit_xor_var() {
    use nom::Finish;
    let span = Span::new("1 ^ 2 && 3");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ExprEnum::And(
                Box::new(Expression::new(
                    ExprEnum::BitXor(
                        Box::new(Expression::new(
                            ExprEnum::NumLiteral(Value::I64(1)),
                            span.subslice(0, 1)
                        )),
                        Box::new(Expression::new(
                            ExprEnum::NumLiteral(Value::I64(2)),
                            span.subslice(4, 1)
                        ))
                    ),
                    span.subslice(0, 5)
                )),
                Box::new(Expression::new(
                    ExprEnum::NumLiteral(Value::I64(3)),
                    span.subslice(9, 1)
                ))
            ),
            span
        )
    );
}

#[test]
fn test_bit_or_arg() {
    use nom::Finish;
    let span = Span::new("a(1 | 2)");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ExprEnum::FnInvoke(
                "a",
                vec![FnArg::new(Expression::new(
                    ExprEnum::BitOr(
                        Box::new(Expression::new(
                            ExprEnum::NumLiteral(Value::I64(1)),
                            span.subslice(2, 1)
                        )),
                        Box::new(Expression::new(
                            ExprEnum::NumLiteral(Value::I64(2)),
                            span.subslice(6, 1)
                        ))
                    ),
                    span.subslice(2, 5)
                ))]
            ),
            span
        )
    );
}

#[test]
fn test_or_expr() {
    let span = Span::new("a < 100");
    assert_eq!(
        or(span).finish().unwrap().1,
        Expression::new(
            LT(
                Box::new(Expression::new(Variable("a"), span.take(1))),
                Box::new(Expression::new(
                    NumLiteral(Value::I64(100)),
                    span.subslice(4, 3)
                ))
            ),
            span,
        )
    );
}

#[test]
fn test_stmt() {
    let span = Span::new("  b  ;");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![Statement::Expression(Expression::new(
            Variable("b"),
            span.subslice(2, 1)
        ))]
    );
}

#[test]
fn test_cond() {
    let span = Span::new("if a < 100 { b }");
    assert_eq!(
        conditional(span).finish().unwrap().1,
        Expression::new(
            Conditional(
                Box::new(Expression::new(
                    LT(
                        Box::new(Expression::new(Variable("a"), span.subslice(3, 1))),
                        Box::new(Expression::new(
                            NumLiteral(Value::I64(100)),
                            span.subslice(7, 3)
                        ))
                    ),
                    span.subslice(3, 8)
                )),
                vec![Statement::Expression(Expression::new(
                    Variable("b"),
                    span.subslice(13, 1)
                ))],
                None
            ),
            span,
        )
    );
}

#[test]
fn test_brace() {
    let span = Span::new("{}");
    assert_eq!(
        brace_expr(span).finish().unwrap().1,
        Expression::new(Brace(vec![]), span)
    );
}

#[test]
fn test_cast() {
    let span = Span::new("a as i32");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(Cast(var_r(span.subslice(0, 1)), TypeDecl::I32), span)
    );
}

#[test]
fn test_tuple() {
    let span = Span::new("(1, \"a\")");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            TupleLiteral(vec![
                Expression::new(ExprEnum::NumLiteral(Value::I64(1)), span.subslice(1, 1)),
                Expression::new(ExprEnum::StrLiteral("a".to_owned()), span.subslice(4, 3))
            ]),
            span
        )
    );
}

#[test]
fn test_type_tuple() {
    let span = Span::new("(i32, str)");
    assert_eq!(
        type_decl(span).finish().unwrap().1,
        TypeDecl::Tuple(vec![TypeDecl::I32, TypeDecl::Str])
    );
}

#[test]
fn test_tuple_index() {
    let span = Span::new("(1, \"a\").1");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            TupleIndex(
                Box::new(Expression::new(
                    TupleLiteral(vec![
                        Expression::new(ExprEnum::NumLiteral(Value::I64(1)), span.subslice(1, 1)),
                        Expression::new(ExprEnum::StrLiteral("a".to_owned()), span.subslice(4, 3))
                    ]),
                    span.subslice(0, 8)
                )),
                1
            ),
            span
        )
    );
}
