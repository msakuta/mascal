use super::*;

#[test]
fn test_comments() {
    assert_eq!(
        Ok(("", Statement::Comment(" x * y "))),
        comment("/* x * y */")
    );
}

#[test]
fn test_ident() {
    assert_eq!(identifier("x123"), Ok(("", "x123")));
}

#[test]
fn test_add() {
    assert_eq!(
        Ok((
            "",
            Statement::Expression(Expression::Add(
                Box::new(Expression::NumLiteral(Value::F64(123.4))),
                Box::new(Expression::NumLiteral(Value::I64(456)))
            ))
        )),
        expression_statement("123.4 + 456")
    );
}

#[test]
fn test_add_paren() {
    assert_eq!(
        Ok((
            "",
            Statement::Expression(Expression::Add(
                Box::new(Expression::NumLiteral(Value::F64(123.4))),
                Box::new(Expression::Add(
                    Box::new(Expression::NumLiteral(Value::I64(456))),
                    Box::new(Expression::NumLiteral(Value::F64(789.5))),
                ))
            ))
        )),
        expression_statement("123.4 + (456 + 789.5)")
    );
}

#[test]
fn str_test() {
    assert_eq!(
        expr("\"hello\""),
        Ok(("", Expression::StrLiteral("hello".to_string())))
    );
    assert_eq!(
        expr("\"sl\\\\ash\""),
        Ok(("", Expression::StrLiteral("sl\\ash".to_string())))
    );
    assert_eq!(
        expr("\"new\\nline\""),
        Ok(("", Expression::StrLiteral("new\nline".to_string())))
    );
}

#[test]
fn expr_test() {
    assert_eq!(
        expr(" 1 +  2 "),
        Ok((
            "",
            Expression::Add(
                Box::new(Expression::NumLiteral(Value::I64(1))),
                Box::new(Expression::NumLiteral(Value::I64(2)))
            )
        ))
    );
    assert_eq!(
        expr(" 12 + 6 - 4+  3"),
        Ok((
            "",
            Expression::Add(
                Box::new(Expression::Sub(
                    Box::new(Expression::Add(
                        Box::new(Expression::NumLiteral(Value::I64(12))),
                        Box::new(Expression::NumLiteral(Value::I64(6))),
                    )),
                    Box::new(Expression::NumLiteral(Value::I64(4))),
                )),
                Box::new(Expression::NumLiteral(Value::I64(3)))
            )
        ))
    );
    assert_eq!(
        expr(" 1 + 2*3 + 4"),
        Ok((
            "",
            Expression::Add(
                Box::new(Expression::Add(
                    Box::new(Expression::NumLiteral(Value::I64(1))),
                    Box::new(Expression::Mult(
                        Box::new(Expression::NumLiteral(Value::I64(2))),
                        Box::new(Expression::NumLiteral(Value::I64(3))),
                    ))
                )),
                Box::new(Expression::NumLiteral(Value::I64(4)))
            )
        ))
    );
}

#[test]
fn parens_test() {
    assert_eq!(
        expr(" (  2 )"),
        Ok(("", Expression::NumLiteral(Value::I64(2))))
    );
    assert_eq!(
        expr(" 2* (  3 + 4 ) "),
        Ok((
            "",
            Expression::Mult(
                Box::new(Expression::NumLiteral(Value::I64(2))),
                Box::new(Expression::Add(
                    Box::new(Expression::NumLiteral(Value::I64(3))),
                    Box::new(Expression::NumLiteral(Value::I64(4))),
                ))
            )
        ))
    );
    assert_eq!(
        expr("  2*2 / ( 5 - 1) + 3"),
        Ok((
            "",
            Expression::Add(
                Box::new(Expression::Div(
                    Box::new(Expression::Mult(
                        Box::new(Expression::NumLiteral(Value::I64(2))),
                        Box::new(Expression::NumLiteral(Value::I64(2))),
                    )),
                    Box::new(Expression::Sub(
                        Box::new(Expression::NumLiteral(Value::I64(5))),
                        Box::new(Expression::NumLiteral(Value::I64(1))),
                    )),
                )),
                Box::new(Expression::NumLiteral(Value::I64(3))),
            )
        ))
    );
}