use crate::{format_stmts, parser::Subslice};

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
    let (_, mut ast) = crate::parser::source(span).unwrap();
    let res = type_check(&mut ast, &mut TypeCheckContext::new(None));
    assert_eq!(
        res,
        Err(TypeCheckError::new(
            "Size is not compatible: 4 is not contained in ..3".to_string(),
            span.subslice(20, 12),
            None
        ))
    );
}

#[test]
fn test_array_range_longer_err() {
    let span = Span::new(r#"var a: [i32; 3..] = [0, 1];"#);
    let (_, mut ast) = crate::parser::source(span).unwrap();
    format_stmts(&ast, &mut std::io::stdout()).unwrap();
    let res = type_check(&mut ast, &mut TypeCheckContext::new(None));
    assert_eq!(
        res,
        Err(TypeCheckError::new(
            "Size is not compatible: 2 is not contained in 3..".to_string(),
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

#[test]
fn test_chain_assign() {
    let span = Span::new(
        r#"var a = 1;
var b = 3;
var c: i32 = 12;

a = b = c;
"#,
    );
    let (_, mut ast) = crate::parser::source(span).unwrap();
    type_check(&mut ast, &mut TypeCheckContext::new(None)).unwrap();

    // TODO: compare AST without spaces or span differences
    let mut buf = vec![0u8; 0];
    format_stmts(&ast, &mut buf).unwrap();
    let inferred = String::from_utf8(buf).unwrap();
    let expected = r#"var a: i32 = 1: i32;
var b: i32 = 3: i32;
var c: i32 = 12: i32;
a = b = c;
"#;
    assert_eq!(inferred, expected);
}

#[test]
fn test_array_propagate() {
    let span = Span::new(
        r#"
    var aa: [i32] = [1,2,3];
    var i = 0;
    i = aa[1];
    "#,
    );
    let (_, mut ast) = crate::parser::source(span).unwrap();
    type_check(&mut ast, &mut TypeCheckContext::new(None)).unwrap();

    // TODO: compare AST without spaces or span differences
    let mut buf = vec![0u8; 0];
    format_stmts(&ast, &mut buf).unwrap();
    let inferred = String::from_utf8(buf).unwrap();
    let expected = r#"var aa: [i32] = [1: i32, 2: i32, 3: i32];
var i: i32 = 0: i32;
i = aa[1: i32|i64];
"#;
    assert_eq!(inferred, expected);
}

#[test]
fn test_tuple_propagate() {
    let span = Span::new(
        r#"
    var i: i32 = 0;
    var a = [1,2,3];
    a[0] = i
    "#,
    );
    let (_, mut ast) = crate::parser::source(span).unwrap();
    type_check(&mut ast, &mut TypeCheckContext::new(None)).unwrap();

    // TODO: compare AST without spaces or span differences
    let mut buf = vec![0u8; 0];
    format_stmts(&ast, &mut buf).unwrap();
    let inferred = String::from_utf8(buf).unwrap();
    let expected = r#"var i: i32 = 0: i32;
var a: [i32; 3] = [1: i32, 2: i32, 3: i32];
a[0: i32|i64] = i;
"#;
    assert_eq!(inferred, expected);
}
