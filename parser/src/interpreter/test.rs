#![cfg(test)]

use nom::{Finish, IResult};

use super::*;
use crate::{
    parser::{
        source, span_source, statement,
        test::{expr_nosemi, expr_semi},
        Subslice,
    },
    type_check,
    type_set::TypeSetAnnotated,
    TypeCheckContext,
};
use ExprEnum::*;

fn span_expr<'a, 'b>(s: &'a str) -> IResult<Span, Expression> {
    expr(Span::new(s))
}

fn eval0(s: &Expression) -> RunResult {
    let mut ctx = EvalContext::default();
    eval(s, &mut ctx).unwrap()
}

fn run0(s: &Vec<Statement>) -> Result<RunResult, EvalError> {
    let mut ctx = EvalContext::default();
    run(s, &mut ctx)
}

#[test]
fn eval_test() {
    assert_eq!(
        eval0(&span_expr(" 1 +  2 ").unwrap().1),
        RunResult::Yield(Value::I64(3))
    );
    assert_eq!(
        eval0(&span_expr(" 12 + 6 - 4+  3").unwrap().1),
        RunResult::Yield(Value::I64(17))
    );
    assert_eq!(
        eval0(&span_expr(" 1 + 2*3 + 4").unwrap().1),
        RunResult::Yield(Value::I64(11))
    );
    assert_eq!(
        eval0(&span_expr(" 1 +  2.5 ").unwrap().1),
        RunResult::Yield(Value::F64(3.5))
    );
}

#[test]
fn parens_eval_test() {
    assert_eq!(
        eval0(&span_expr(" (  2 )").unwrap().1),
        RunResult::Yield(Value::I64(2))
    );
    assert_eq!(
        eval0(&span_expr(" 2* (  3 + 4 ) ").unwrap().1),
        RunResult::Yield(Value::I64(14))
    );
    assert_eq!(
        eval0(&span_expr("  2*2 / ( 5 - 1) + 3").unwrap().1),
        RunResult::Yield(Value::I64(4))
    );
}

#[test]
fn var_ident_test() {
    let span = Span::new(" x123 ");
    let res = _var_ref(span).finish().unwrap().1;
    assert_eq!(res, Expression::new(Variable("x123"), span.subslice(1, 4)));
}

#[test]
fn var_test() {
    let mut ctx = EvalContext::default();
    ctx.variables
        .borrow_mut()
        .insert("x", Rc::new(RefCell::new(Value::F64(42.))));
    assert_eq!(
        eval(&span_expr(" x +  2 ").unwrap().1, &mut ctx).unwrap(),
        RunResult::Yield(Value::F64(44.))
    );
}

fn var_r(name: Span) -> Box<Expression> {
    Box::new(Expression::new(Variable(*name), name))
}

/// Boxed numeric literal
fn bnl(value: Value, span: Span) -> Box<Expression> {
    Box::new(Expression::new(
        NumLiteral(value, TypeSetAnnotated::int()),
        span,
    ))
}

#[test]
fn var_assign_test() {
    let mut ctx = EvalContext::default();
    ctx.variables
        .borrow_mut()
        .insert("x", Rc::new(RefCell::new(Value::F64(42.))));
    let span = Span::new("x=12");
    assert_eq!(
        var_assign(span).finish().unwrap().1,
        Expression::new(
            VarAssign(
                var_r(span.subslice(0, 1)),
                bnl(Value::I64(12), span.subslice(2, 2))
            ),
            span
        )
    );
    assert_eq!(
        eval(&var_assign(Span::new("x=12")).finish().unwrap().1, &mut ctx).unwrap(),
        RunResult::Yield(Value::I64(12))
    );
}

#[test]
fn fn_invoke_test() {
    let span = Span::new("f();");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![Statement::Expression {
            ex: Expression::new(FnInvoke("f", vec![]), span.subslice(0, 3)),
            semicolon: true
        }]
    );
    let span = Span::new("f(1);");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![Statement::Expression {
            ex: Expression::new(
                FnInvoke(
                    "f",
                    vec![FnArg {
                        name: None,
                        expr: *bnl(Value::I64(1), span.subslice(2, 1))
                    }]
                ),
                span.subslice(0, 4)
            ),
            semicolon: true
        }]
    );
}

#[test]
fn fn_default_test() {
    let span = Span::new("fn a(a: i32 = 1) { a; }");
    let stmts = source(span).finish();
    assert!(stmts.is_ok());
}

/// Tests non-const default argument expression will fail to evaluate
#[test]
fn fn_default_failure_test() {
    let span = Span::new("var b = 1; fn f(a: i32 = b) { a; } f()");
    let stmts = source(span).finish().unwrap().1;
    let Err(err) = run(&stmts, &mut EvalContext::default()) else {
        panic!("Should error");
    };
    assert!(matches!(err, EvalError::VarNotFound(_)));
}

fn span_conditional(s: &str) -> IResult<Span, Expression> {
    conditional(Span::new(s))
}

#[test]
fn cond_test() {
    let span = Span::new("if 0 { 1; }");
    assert_eq!(
        conditional(span).finish().unwrap().1,
        Expression::new(
            Conditional(
                bnl(Value::I64(0), span.subslice(3, 1)),
                vec![Statement::Expression {
                    ex: *bnl(Value::I64(1), span.subslice(7, 1)),
                    semicolon: true
                }],
                None,
            ),
            span
        )
    );
    let span = Span::new("if (1) { 2; } else { 3; }");
    assert_eq!(
        conditional(span).finish().unwrap().1,
        Expression::new(
            Conditional(
                bnl(Value::I64(1), span.subslice(3, 4)),
                vec![expr_semi(*bnl(Value::I64(2), span.subslice(9, 1)))],
                Some(vec![expr_semi(*bnl(Value::I64(3), span.subslice(21, 1)))]),
            ),
            span
        )
    );
    let span = Span::new("if 1 && 2 { 2; } else { 3; }");
    assert_eq!(
        conditional(span).finish().unwrap().1,
        Expression::new(
            Conditional(
                Box::new(Expression::new(
                    And(
                        bnl(Value::I64(1), span.subslice(3, 1)),
                        bnl(Value::I64(2), span.subslice(8, 1)),
                    ),
                    span.subslice(3, 6)
                )),
                vec![expr_semi(*bnl(Value::I64(2), span.subslice(12, 1)))],
                Some(vec![expr_semi(*bnl(Value::I64(3), span.subslice(24, 1)))]),
            ),
            span
        )
    );
}

#[test]
fn cond_eval_test() {
    assert_eq!(
        eval0(&span_conditional("if 0 { 1; }").finish().unwrap().1),
        RunResult::Yield(Value::I32(0))
    );
    assert_eq!(
        eval0(
            &span_conditional("if (1) { 2; } else { 3;}")
                .finish()
                .unwrap()
                .1
        ),
        RunResult::Yield(Value::I64(2))
    );
    assert_eq!(
        eval0(
            &span_conditional("if (0) { 2; } else { 3;}")
                .finish()
                .unwrap()
                .1
        ),
        RunResult::Yield(Value::I64(3))
    );
}

#[test]
fn cmp_test() {
    let span = Span::new(" 1 <  2 ");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            LT(
                bnl(Value::I64(1), span.subslice(1, 1)),
                bnl(Value::I64(2), span.subslice(6, 1))
            ),
            span
        )
    );
    let span = Span::new(" 1 > 2");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            GT(
                bnl(Value::I64(1), span.subslice(1, 1)),
                bnl(Value::I64(2), span.subslice(5, 1))
            ),
            span
        )
    );
}

#[test]
fn cmp_eval_test() {
    assert_eq!(
        eval0(&cmp_expr(Span::new(" 1 <  2 ")).finish().unwrap().1),
        RunResult::Yield(Value::I32(1))
    );
    assert_eq!(
        eval0(&cmp_expr(Span::new(" 1 > 2")).finish().unwrap().1),
        RunResult::Yield(Value::I32(0))
    );
    assert_eq!(
        eval0(&cmp_expr(Span::new(" 1 <=  2 ")).finish().unwrap().1),
        RunResult::Yield(Value::I32(1))
    );
    assert_eq!(
        eval0(&cmp_expr(Span::new(" 1 >= 2")).finish().unwrap().1),
        RunResult::Yield(Value::I32(0))
    );
    assert_eq!(
        eval0(&cmp_expr(Span::new(" 2 < 1")).finish().unwrap().1),
        RunResult::Yield(Value::I32(0))
    );
    assert_eq!(
        eval0(&cmp_expr(Span::new(" 2 > 1")).finish().unwrap().1),
        RunResult::Yield(Value::I32(1))
    );
    assert_eq!(
        eval0(&cmp_expr(Span::new(" 2 <= 1")).finish().unwrap().1),
        RunResult::Yield(Value::I32(0))
    );
    assert_eq!(
        eval0(&cmp_expr(Span::new(" 2 >= 1")).finish().unwrap().1),
        RunResult::Yield(Value::I32(1))
    );
}

#[test]
fn logic_test() {
    let span = Span::new(" 0 && 1 ");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            And(
                bnl(Value::I64(0), span.subslice(1, 1)),
                bnl(Value::I64(1), span.subslice(6, 1))
            ),
            span.subslice(1, 6)
        )
    );
    let span = Span::new(" 1 || 2");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            Or(
                bnl(Value::I64(1), span.subslice(1, 1)),
                bnl(Value::I64(2), span.subslice(6, 1))
            ),
            span.subslice(1, 6)
        )
    );
    let span = Span::new(" 0 && 1 && 2 ");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            And(
                Box::new(Expression::new(
                    And(
                        bnl(Value::I64(0), span.subslice(1, 1)),
                        bnl(Value::I64(1), span.subslice(6, 1)),
                    ),
                    span.subslice(1, 6),
                )),
                bnl(Value::I64(2), span.subslice(11, 1)),
            ),
            span.subslice(1, 11)
        )
    );
    let span = Span::new("0 || 1 || 2 ");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            Or(
                Box::new(Expression::new(
                    Or(
                        bnl(Value::I64(0), span.subslice(0, 1)),
                        bnl(Value::I64(1), span.subslice(5, 1)),
                    ),
                    span.subslice(0, 6),
                )),
                bnl(Value::I64(2), span.subslice(10, 1)),
            ),
            span.subslice(0, 11)
        )
    );
    let span = Span::new(" 1 && 2 || 3 && 4");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            Or(
                Box::new(Expression::new(
                    And(
                        bnl(Value::I64(1), span.subslice(1, 1)),
                        bnl(Value::I64(2), span.subslice(6, 1))
                    ),
                    span.subslice(1, 6)
                )),
                Box::new(Expression::new(
                    And(
                        bnl(Value::I64(3), span.subslice(11, 1)),
                        bnl(Value::I64(4), span.subslice(16, 1))
                    ),
                    span.subslice(11, 6)
                )),
            ),
            span.subslice(1, 16)
        )
    );
    let span = Span::new(" 1 || !1");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            Or(
                bnl(Value::I64(1), span.subslice(1, 1)),
                Box::new(Expression::new(
                    Not(bnl(Value::I64(1), span.subslice(7, 1))),
                    span.subslice(6, 2)
                ))
            ),
            span.subslice(1, 7)
        )
    );
    let span = Span::new(" !!1");
    assert_eq!(
        conditional_expr(span).finish().unwrap().1,
        Expression::new(
            Not(Box::new(Expression::new(
                Not(bnl(Value::I64(1), span.subslice(3, 1))),
                span.subslice(2, 2)
            ))),
            span.subslice(0, 4)
        )
    );
}

fn span_full_expression(s: &str) -> IResult<Span, Expression> {
    full_expression(Span::new(s))
}

#[test]
fn logic_eval_test() {
    assert_eq!(
        eval0(&span_full_expression(" 0 && 1 ").finish().unwrap().1),
        RunResult::Yield(Value::I32(0))
    );
    assert_eq!(
        eval0(&span_full_expression(" 0 || 1 ").finish().unwrap().1),
        RunResult::Yield(Value::I32(1))
    );
    assert_eq!(
        eval0(&span_full_expression(" 1 && 0 || 1 ").finish().unwrap().1),
        RunResult::Yield(Value::I32(1))
    );
    assert_eq!(
        eval0(&span_full_expression(" 1 && 0 || 0 ").finish().unwrap().1),
        RunResult::Yield(Value::I32(0))
    );
    assert_eq!(
        eval0(&span_full_expression(" 1 && !0 ").finish().unwrap().1),
        RunResult::Yield(Value::I32(1))
    );
}

/// Numeric literal without box
fn nl(value: Value, span: Span) -> Expression {
    Expression::new(NumLiteral(value, TypeSetAnnotated::int()), span)
}

#[test]
fn brace_expr_test() {
    let span = Span::new(" { 1; }");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            Brace(vec![expr_semi(nl(Value::I64(1), span.subslice(3, 1)))]),
            span.subslice(1, 6)
        )
    );
    let span = Span::new(" { 1; 2; }");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            Brace(vec![
                expr_semi(nl(Value::I64(1), span.subslice(3, 1))),
                expr_semi(nl(Value::I64(2), span.subslice(6, 1))),
            ]),
            span.subslice(1, 9)
        )
    );
    let span = Span::new(" { 1; 2 }");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            Brace(vec![
                expr_semi(nl(Value::I64(1), span.subslice(3, 1))),
                expr_nosemi(nl(Value::I64(2), span.subslice(6, 1))),
            ]),
            span.subslice(1, 8)
        )
    );
    let span = Span::new(" { x = 1; x }; ");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![expr_semi(Expression::new(
            Brace(vec![
                expr_semi(Expression::new(
                    VarAssign(
                        var_r(span.subslice(3, 1)),
                        bnl(Value::I64(1), span.subslice(7, 1))
                    ),
                    span.subslice(3, 5)
                )),
                expr_nosemi(*var_r(span.subslice(10, 1))),
            ]),
            span.subslice(1, 12)
        ))]
    );
}

#[test]
fn brace_expr_eval_test() {
    assert_eq!(
        eval0(&span_full_expression(" { 1; } ").unwrap().1),
        RunResult::Yield(Value::I64(1))
    );
    assert_eq!(
        eval0(&span_full_expression(" { 1; 2 }").unwrap().1),
        RunResult::Yield(Value::I64(2))
    );
    assert_eq!(
        eval0(&span_full_expression(" {1; 2;} ").unwrap().1),
        RunResult::Yield(Value::I64(2))
    );
    assert_eq!(
        eval0(
            &span_full_expression("  { var x: i64 = 0; x = 1; x } ")
                .unwrap()
                .1
        ),
        RunResult::Yield(Value::I64(1))
    );
}

#[test]
fn stmt_test() {
    let span = Span::new(" 1;");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![expr_semi(nl(Value::I64(1), span.subslice(1, 1)))],
    );
    let span = Span::new(" 1 ");
    assert_eq!(
        statement(span).finish().unwrap().1,
        expr_nosemi(nl(Value::I64(1), span.subslice(1, 1))),
    );
    let span = Span::new(" 1 ");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![expr_nosemi(nl(Value::I64(1), span.subslice(1, 1)))],
    );
}

#[test]
fn stmts_test() {
    let span = Span::new(" 1; 2 ");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![
            expr_semi(nl(Value::I64(1), span.subslice(1, 1))),
            expr_nosemi(nl(Value::I64(2), span.subslice(4, 1))),
        ]
    );
    let span = Span::new(" 1; 2; ");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![
            expr_semi(nl(Value::I64(1), span.subslice(1, 1))),
            expr_semi(nl(Value::I64(2), span.subslice(4, 1))),
        ]
    );
}

#[test]
fn array_decl_test() {
    assert_eq!(
        type_spec(Span::new(": i32")).finish().unwrap().1,
        TypeDecl::I32
    );
    assert_eq!(
        type_spec(Span::new(": [i32]")).finish().unwrap().1,
        TypeDecl::Array(Box::new(TypeDecl::I32), ArraySize::Any)
    );
    assert_eq!(
        type_spec(Span::new(": [[f32]]")).finish().unwrap().1,
        TypeDecl::Array(
            Box::new(TypeDecl::Array(Box::new(TypeDecl::F32), ArraySize::Any)),
            ArraySize::Any
        )
    );
    assert_eq!(
        type_spec(Span::new(": [f32; 3]")).finish().unwrap().1,
        TypeDecl::Array(Box::new(TypeDecl::F32), ArraySize::Fixed(3))
    );
}

#[test]
fn array_literal_test() {
    use Value::*;
    let span = Span::new("[1,3,5]");
    assert_eq!(
        array_literal(span).finish().unwrap().1,
        Expression::new(
            ArrLiteral(vec![
                nl(I64(1), span.subslice(1, 1)),
                nl(I64(3), span.subslice(3, 1)),
                nl(I64(5), span.subslice(5, 1))
            ]),
            span
        )
    );
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ArrLiteral(vec![
                nl(I64(1), span.subslice(1, 1)),
                nl(I64(3), span.subslice(3, 1)),
                nl(I64(5), span.subslice(5, 1))
            ]),
            span
        )
    );
    let span = Span::new("[[1,3,5],[7,8,9]]");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ArrLiteral(vec![
                Expression::new(
                    ArrLiteral(vec![
                        nl(I64(1), span.subslice(2, 1)),
                        nl(I64(3), span.subslice(4, 1)),
                        nl(I64(5), span.subslice(6, 1))
                    ]),
                    span.subslice(1, 7)
                ),
                Expression::new(
                    ArrLiteral(vec![
                        nl(I64(7), span.subslice(10, 1)),
                        nl(I64(8), span.subslice(12, 1)),
                        nl(I64(9), span.subslice(14, 1))
                    ]),
                    span.subslice(9, 7)
                ),
            ]),
            span
        )
    );
}

#[test]
fn array_literal_eval_test() {
    #![allow(dead_code)]
    use Value::*;
    fn i64(i: i64) -> Value {
        I64(i)
    }
    fn f64(i: f64) -> Value {
        F64(i)
    }
    assert_eq!(
        eval0(&span_full_expression("[1,3,5]").finish().unwrap().1),
        // Right now array literals have "Any" internal type, but it should be decided somehow.
        RunResult::Yield(Value::Array(ArrayInt::new(
            TypeDecl::Any,
            vec![i64(1), i64(3), i64(5)]
        )))
    );

    // Type coercion won't happen through variable declaration, because it is discarded in compiled bytecode.
    assert_eq!(
        run0(&span_source("var v: [f64] = [1,3,5]; v").finish().unwrap().1).unwrap(),
        RunResult::Yield(Value::Array(ArrayInt::new(
            TypeDecl::Any,
            vec![i64(1), i64(3), i64(5)]
        )))
    );
}

#[test]
fn fn_array_decl_test() {
    let span = Span::new("fn f(a: [i32]) { x = 123; }");
    assert_eq!(
        func_decl(span).finish().unwrap().1,
        Statement::FnDecl {
            name: span.subslice(3, 1),
            args: vec![ArgDecl::new(
                span.subslice(5, 1),
                TypeDecl::Array(Box::new(TypeDecl::I32), ArraySize::Any)
            )],
            ret_type: RetType::Void,
            stmts: Rc::new(vec![expr_semi(Expression::new(
                VarAssign(
                    var_r(span.subslice(17, 1)),
                    bnl(Value::I64(123), span.subslice(21, 3))
                ),
                span.subslice(17, 7)
            ))])
        }
    );
}

#[test]
fn array_index_test() {
    use Value::*;
    let span = Span::new("a[1]");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ArrIndex(
                var_r(span.subslice(0, 1)),
                vec![nl(I64(1), span.subslice(2, 1))]
            ),
            span
        )
    );
    let span = Span::new("b[1,3,5]");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            ArrIndex(
                var_r(span.subslice(0, 1)),
                vec![
                    nl(I64(1), span.subslice(2, 1)),
                    nl(I64(3), span.subslice(4, 1)),
                    nl(I64(5), span.subslice(6, 1))
                ]
            ),
            span
        )
    );
}

#[test]
fn array_index_eval_test() {
    use Value::*;
    let mut ctx = EvalContext::default();

    // This is very verbose, but necessary to match against a variable in ctx.variables.
    let ast = span_source("var a: [i32] = [1,3,5]; a[1]").unwrap().1;
    let run_result = run(&ast, &mut ctx);
    let a_ref = ctx._get_var_rc("a").unwrap();
    let mut a_rc = None;
    // Very ugly idiom to extract a clone of a variant in a RefCell
    std::cell::Ref::map(a_ref.borrow(), |v| match v {
        Value::Array(a) => {
            a_rc = Some(a.borrow().values[1].clone());
            &()
        }
        _ => panic!("a must be an array"),
    });

    assert_eq!(run_result.unwrap(), RunResult::Yield(a_rc.unwrap()));

    // Technically, this test will return a reference to an element in a temporary array,
    // but we wouldn't care and just unwrap_deref.
    assert_eq!(
        run0(&span_source("[1,3,5][1]").unwrap().1)
            .and_then(unwrap_deref)
            .unwrap(),
        RunResult::Yield(I64(3))
    );
    assert_eq!(
        run0(&span_source("len([1,3,5])").unwrap().1).unwrap(),
        RunResult::Yield(I64(3))
    );
}

#[test]
fn array_index_assign_test() {
    use Value::*;
    let span = Span::new("a[0] = b[0]");
    assert_eq!(
        full_expression(span).finish().unwrap().1,
        Expression::new(
            VarAssign(
                Box::new(Expression::new(
                    ArrIndex(
                        var_r(span.subslice(0, 1)),
                        vec![nl(I64(0), span.subslice(2, 1))]
                    ),
                    span.subslice(0, 5)
                )),
                Box::new(Expression::new(
                    ArrIndex(
                        var_r(span.subslice(7, 1)),
                        vec![nl(I64(0), span.subslice(9, 1))]
                    ),
                    span.subslice(7, 4)
                )),
            ),
            span
        )
    );
    assert_eq!(
        run0(&span_source("var a: [i32] = [1,3,5]; a[1] = 123").unwrap().1).unwrap(),
        RunResult::Yield(I64(123))
    );
}

#[test]
fn array_sized_test() {
    let span = Span::new("var a: [i32; 3] = [1,2,3]; var b: [i32; 3] = [4,5,6]; a = b;");
    let mut ast = source(span).finish().unwrap().1;
    type_check(&mut ast, &mut TypeCheckContext::new(None)).unwrap();
    run0(&ast).unwrap();
}

#[test]
fn array_sized_error_test() {
    let span = Span::new("var a: [i32; 3] = [1,2,3]; var b: [i32; 4] = [4,5,6,7]; a = b;");
    let mut ast = source(span).finish().unwrap().1;
    match type_check(&mut ast, &mut TypeCheckContext::new(Some("input"))) {
        Ok(_) => panic!(),
        Err(e) => assert_eq!(
            e.to_string(),
            "Array size is not compatible: 4 cannot assign to 3\ninput:1:57"
        ),
    }
    // It will run successfully although the typecheck fails.
    run0(&ast).unwrap();
}

#[test]
fn array_sized_cmp_error_test() {
    let span = Span::new("var a: [i32; 3] = [1,2,3]; var b: [i32; 4] = [4,5,6,7]; a < b;");
    let mut ast = source(span).finish().unwrap().1;
    match type_check(&mut ast, &mut TypeCheckContext::new(Some("input"))) {
        Ok(_) => panic!("type check succeeded when it should fail: {:?}", ast),
        Err(e) => assert_eq!(e.to_string(), "Operation LT between incompatible type [i32; 3] and [i32; 4]: Comparison between incompatible types: [i32; 3] and [i32; 4]: Array size is not compatible: 4 cannot assign to 3\ninput:1:57"),
    }
    // It will fail at runtime
    assert!(run0(&ast).is_err());
}

#[test]
fn var_decl_test() {
    use Statement::VarDecl;
    let span = Span::new(" var x; x = 0;");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![
            VarDecl {
                name: span.subslice(5, 1),
                ty: TypeDecl::Any,
                ty_annotated: false,
                init: None
            },
            expr_semi(Expression::new(
                VarAssign(
                    var_r(span.subslice(8, 1)),
                    bnl(Value::I64(0), span.subslice(12, 1))
                ),
                span.subslice(7, 6)
            )),
        ]
    );
    let span = Span::new(" var x = 0;");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![VarDecl {
            name: span.subslice(5, 1),
            ty: TypeDecl::Any,
            ty_annotated: false,
            init: Some(nl(Value::I64(0), span.subslice(9, 1)))
        }]
    );
    let span = Span::new(" var x: f64 = 0;");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![VarDecl {
            name: span.subslice(5, 1),
            ty: TypeDecl::F64,
            ty_annotated: true,
            init: Some(nl(Value::I64(0), span.subslice(14, 1)))
        }]
    );
    let span = Span::new(" var x: f32 = 0;");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![VarDecl {
            name: span.subslice(5, 1),
            ty: TypeDecl::F32,
            ty_annotated: true,
            init: Some(nl(Value::I64(0), span.subslice(14, 1))),
        }]
    );
    let span = Span::new(" var x: i64 = 0;");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![VarDecl {
            name: span.subslice(5, 1),
            ty: TypeDecl::I64,
            ty_annotated: true,
            init: Some(nl(Value::I64(0), span.subslice(14, 1)))
        }]
    );
    let span = Span::new(" var x: i32 = 0;");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![VarDecl {
            name: span.subslice(5, 1),
            ty: TypeDecl::I32,
            ty_annotated: true,
            init: Some(nl(Value::I64(0), span.subslice(14, 1)))
        }]
    );
}

#[test]
fn loop_test() {
    let span = Span::new(" var i; i = 0; loop { i = i + 1; }");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![
            Statement::VarDecl {
                name: span.subslice(5, 1),
                ty: TypeDecl::Any,
                ty_annotated: false,
                init: None
            },
            expr_semi(Expression::new(
                VarAssign(
                    var_r(span.subslice(8, 1)),
                    bnl(Value::I64(0), span.subslice(12, 1))
                ),
                span.subslice(7, 6)
            )),
            Statement::Loop(vec![expr_semi(Expression::new(
                VarAssign(
                    var_r(span.subslice(22, 1)),
                    Box::new(Expression::new(
                        Add(
                            var_r(span.subslice(26, 1)),
                            bnl(Value::I64(1), span.subslice(30, 1)),
                        ),
                        span.subslice(26, 5)
                    ))
                ),
                span.subslice(22, 9)
            ))])
        ]
    );
    let span = Span::new("if i < 10 { break };");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![expr_semi(Expression::new(
            Conditional(
                Box::new(Expression::new(
                    LT(
                        var_r(span.subslice(3, 1)),
                        bnl(Value::I64(10), span.subslice(7, 2)),
                    ),
                    span.subslice(3, 7)
                )),
                vec![Statement::Break],
                None,
            ),
            span.subslice(0, 19)
        ))]
    );
    let span = Span::new(" var i; i = 0; loop { i = i + 1; if i < 10 { break }; }");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![
            Statement::VarDecl {
                name: span.subslice(5, 1),
                ty: TypeDecl::Any,
                ty_annotated: false,
                init: None
            },
            expr_semi(Expression::new(
                VarAssign(
                    var_r(span.subslice(8, 1)),
                    bnl(Value::I64(0), span.subslice(12, 1))
                ),
                span.subslice(7, 6)
            )),
            Statement::Loop(vec![
                expr_semi(Expression::new(
                    VarAssign(
                        var_r(span.subslice(22, 1)),
                        Box::new(Expression::new(
                            Add(
                                var_r(span.subslice(26, 1)),
                                bnl(Value::I64(1), span.subslice(30, 1)),
                            ),
                            span.subslice(26, 5)
                        ))
                    ),
                    span.subslice(22, 9)
                )),
                expr_semi(Expression::new(
                    Conditional(
                        Box::new(Expression::new(
                            LT(
                                var_r(span.subslice(36, 1)),
                                bnl(Value::I64(10), span.subslice(40, 2)),
                            ),
                            span.subslice(36, 7)
                        )),
                        vec![Statement::Break],
                        None
                    ),
                    span.subslice(33, 19)
                ))
            ])
        ]
    );
}

#[test]
fn while_test() {
    let span = Span::new(" var i: i64; i = 0; while i < 10 { i = i + 1; }");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![
            Statement::VarDecl {
                name: span.subslice(5, 1),
                ty: TypeDecl::I64,
                ty_annotated: true,
                init: None
            },
            expr_semi(Expression::new(
                VarAssign(
                    var_r(span.subslice(13, 1)),
                    bnl(Value::I64(0), span.subslice(17, 1))
                ),
                span.subslice(12, 6)
            )),
            Statement::While(
                Expression::new(
                    LT(
                        var_r(span.subslice(26, 1)),
                        bnl(Value::I64(10), span.subslice(30, 2)),
                    ),
                    span.subslice(26, 7)
                ),
                vec![expr_semi(Expression::new(
                    VarAssign(
                        var_r(span.subslice(35, 1)),
                        Box::new(Expression::new(
                            Add(
                                var_r(span.subslice(39, 1)),
                                bnl(Value::I64(1), span.subslice(43, 1)),
                            ),
                            span.subslice(39, 5)
                        ))
                    ),
                    span.subslice(35, 9)
                ))]
            )
        ]
    );
}

#[test]
fn for_test() {
    let span = Span::new(" for i in 0..10 { print(i); }");
    assert_eq!(
        source(span).finish().unwrap().1,
        vec![Statement::For {
            var: span.subslice(5, 1),
            ty: None,
            start: Expression::new(
                NumLiteral(Value::I64(0), TypeSetAnnotated::int()),
                span.subslice(10, 1)
            ),
            end: Expression::new(
                NumLiteral(Value::I64(10), TypeSetAnnotated::int()),
                span.subslice(13, 2)
            ),
            stmts: vec![expr_semi(Expression::new(
                FnInvoke("print", vec![FnArg::new(*var_r(span.subslice(24, 1)))],),
                span.subslice(18, 8)
            ))]
        }]
    );
}
