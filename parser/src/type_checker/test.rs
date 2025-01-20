use crate::parser::Subslice;

use super::*;

#[test]
fn test_cast() {
    let span = Span::new("var a: i64; a as i32");
    let (_, ast) = crate::parser::source(span).unwrap();
    assert_eq!(
        tc_stmts_forward(&ast, &mut TypeCheckContext::new(None)).unwrap(),
        TypeSet::i32()
    );
}

#[test]
fn test_named_arg() {
    let span = Span::new("fn f(a: i32, b: str) -> i32 { a }\n f(b: \"hey\", a: 42)");
    let (_, ast) = crate::parser::source(span).unwrap();
    assert_eq!(
        tc_stmts_forward(&ast, &mut TypeCheckContext::new(None)).unwrap(),
        TypeSet::i32()
    );
}

#[test]
fn test_tuple_index() {
    let span = Span::new(r#"var a: (i32, str, f64) = (42, "a", 3.14); a.0"#);
    let (_, ast) = crate::parser::source(span).unwrap();
    assert_eq!(
        tc_stmts_forward(&ast, &mut TypeCheckContext::new(None)).unwrap(),
        TypeSet::i32()
    );
}

#[test]
fn test_tuple_index_err() {
    let span = Span::new(r#"var a: (i32, str, f64) = (42, "a", 3.14); a.3"#);
    let (_, ast) = crate::parser::source(span).unwrap();
    let res = tc_stmts_forward(&ast, &mut TypeCheckContext::new(None));
    assert_eq!(
        res,
        Err(TypeCheckError::new(
            "Tuple index out of range".to_string(),
            span.subslice(42, 1),
            None
        ))
    );
}

#[test]
fn test_array_range_shorter() {
    let span = Span::new(r#"var a: [i32; ..3] = [0, 1]; a[0]"#);
    let (_, ast) = crate::parser::source(span).unwrap();
    assert_eq!(
        tc_stmts_forward(&ast, &mut TypeCheckContext::new(None)).unwrap(),
        TypeSet::i32()
    );
}

#[test]
fn test_array_range_longer() {
    let span = Span::new(r#"var a: [i32; 3..] = [0, 1, 2, 3]; a[0]"#);
    let (_, ast) = crate::parser::source(span).unwrap();
    assert_eq!(
        tc_stmts_forward(&ast, &mut TypeCheckContext::new(None)).unwrap(),
        TypeSet::i32()
    );
}

#[test]
fn test_array_range_shorter_err() {
    let span = Span::new(r#"var a: [i32; ..3] = [0, 1, 2, 3];"#);
    let (_, ast) = crate::parser::source(span).unwrap();
    let res = tc_stmts_forward(&ast, &mut TypeCheckContext::new(None));
    assert_eq!(
        res,
        Err(TypeCheckError::new(
            "Array range is not compatible: 4 cannot assign to ..3".to_string(),
            span.subslice(20, 12),
            None
        ))
    );
}

#[test]
fn test_array_range_longer_err() {
    let span = Span::new(r#"var a: [i32; 3..] = [0, 1];"#);
    let (_, ast) = crate::parser::source(span).unwrap();
    let res = tc_stmts_forward(&ast, &mut TypeCheckContext::new(None));
    assert_eq!(
        res,
        Err(TypeCheckError::new(
            "Array range is not compatible: 2 cannot assign to 3..".to_string(),
            span.subslice(20, 6),
            None
        ))
    );
}

#[test]
fn test_array_range_full() {
    let span = Span::new(r#"var a: [i32; ..] = [0, 1, 2, 3]; a[0]"#);
    let (_, ast) = crate::parser::source(span).unwrap();
    assert_eq!(
        tc_stmts_forward(&ast, &mut TypeCheckContext::new(None)).unwrap(),
        TypeSet::i32()
    );
}
