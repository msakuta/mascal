use super::*;
use crate::{
    compile,
    compiler::CompileError,
    parser::{expr, span_source, test::expr_nosemi},
    value::ArrayInt,
    Span, TypeDecl,
};

fn compile_expr(s: &str) -> Bytecode {
    let bytecode = compile(
        &[expr_nosemi(expr(Span::new(s)).unwrap().1)],
        HashMap::new(),
    )
    .unwrap();
    bytecode
}

#[test]
fn eval_test() {
    assert_eq!(interpret(&compile_expr(" 1 +  2 ")).unwrap(), Value::I64(3));
    assert_eq!(
        interpret(&compile_expr(" 12 + 6 - 4+  3")).unwrap(),
        Value::I64(17)
    );
    assert_eq!(
        interpret(&compile_expr(" 1 + 2*3 + 4")).unwrap(),
        Value::I64(11)
    );
    assert_eq!(
        interpret(&compile_expr(" 1 +  2.5 ")).unwrap(),
        Value::F64(3.5)
    );
}

#[test]
fn parens_eval_test() {
    assert_eq!(interpret(&compile_expr(" (  2 )")).unwrap(), Value::I64(2));
    assert_eq!(
        interpret(&compile_expr(" 2* (  3 + 4 ) ")).unwrap(),
        Value::I64(14)
    );
    assert_eq!(
        interpret(&compile_expr("  2*2 / ( 5 - 1) + 3")).unwrap(),
        Value::I64(4)
    );
}

fn compile_and_run(src: &str) -> Result<Value, CompileError<'_>> {
    Ok(interpret(
        &compile(&span_source(src).unwrap().1, HashMap::new()).unwrap(),
    )?)
}

#[test]
fn var_test() {
    assert_eq!(
        compile_and_run("var x = 42.; x +  2 ").unwrap(),
        Value::F64(44.)
    );
}

#[test]
fn var_assign_test() {
    assert_eq!(
        compile_and_run("var x = 42.; x=12").unwrap(),
        Value::I64(12)
    );
}

#[test]
fn cond_test() {
    assert_eq!(compile_and_run("if 0 { 1; }").unwrap(), Value::I64(0));
    assert_eq!(
        compile_and_run("if (1) { 2 } else { 3 }").unwrap(),
        Value::I64(2)
    );
    assert_eq!(
        compile_and_run("if 1 && 2 { 2 } else { 3 }").unwrap(),
        Value::I64(2)
    );
}

#[test]
fn cmp_eval_test() {
    assert_eq!(compile_and_run(" 1 <  2 ").unwrap(), Value::I32(1));
    assert_eq!(compile_and_run(" 1 > 2").unwrap(), Value::I32(0));
    assert_eq!(compile_and_run(" 2 < 1").unwrap(), Value::I32(0));
    assert_eq!(compile_and_run(" 2 > 1").unwrap(), Value::I32(1));
}

#[test]
fn bit_op_test() {
    assert_eq!(compile_and_run(" 0 & 1 ").unwrap(), Value::I64(0));
    assert_eq!(compile_and_run(" 0 | 1 ").unwrap(), Value::I64(1));
    assert_eq!(compile_and_run(" 1 & 0 | 1 ").unwrap(), Value::I64(1));
    assert_eq!(compile_and_run(" 1 & 0 | 0 ").unwrap(), Value::I64(0));
    assert_eq!(compile_and_run(" 1 & !0 ").unwrap(), Value::I64(1));
    assert_eq!(compile_and_run(" 1 ^ 2 ").unwrap(), Value::I64(3));
    assert_eq!(compile_and_run(" 3 ^ 2 ").unwrap(), Value::I64(1));
}

#[test]
fn logic_eval_test() {
    assert_eq!(compile_and_run(" 0 && 1 ").unwrap(), Value::I32(0));
    assert_eq!(compile_and_run(" 0 || 1 ").unwrap(), Value::I32(1));
    assert_eq!(compile_and_run(" 1 && 0 || 1 ").unwrap(), Value::I32(1));
    assert_eq!(compile_and_run(" 1 && 0 || 0 ").unwrap(), Value::I32(0));
    assert_eq!(compile_and_run(" 1 && !0 ").unwrap(), Value::I32(1));
}

#[test]
fn brace_expr_eval_test() {
    assert_eq!(compile_and_run(" { 1; } ").unwrap(), Value::I64(0));
    assert_eq!(compile_and_run(" { 1; 2 }").unwrap(), Value::I64(2));
    assert_eq!(compile_and_run(" {1; 2;} ").unwrap(), Value::I64(0));
    assert_eq!(
        compile_and_run("  { var x: i64 = 10; x = 20; x } ").unwrap(),
        Value::I64(20)
    );
}

#[test]
fn brace_shadowing_test() {
    assert_eq!(
        compile_and_run(" var x = 0; { var x = 1; }; x").unwrap(),
        Value::I64(0)
    );
    assert_eq!(
        compile_and_run(" var x = 0; { var x = 1; x }").unwrap(),
        Value::I64(1)
    );
    assert_eq!(
        compile_and_run(" var x = 0; { var x = 1; x = 2; }; x").unwrap(),
        Value::I64(0)
    );
}

fn compile_and_run_with(src: &str, fun: impl Fn(&[Value]) + 'static) -> Result<Value, EvalError> {
    let mut bytecode = compile(&span_source(src).unwrap().1, HashMap::new()).unwrap();
    bytecode.add_ext_fn(
        "print".to_string(),
        Box::new(move |vals| {
            fun(vals);
            Ok(Value::I64(0))
        }),
    );
    interpret(&bytecode)
}

#[test]
fn ext_fn_call() {
    // The return value does not matter
    assert!(matches!(
        compile_and_run_with("print(1 + 2);", |vals| {
            assert_eq!(vals[0], Value::I64(3));
        }),
        Ok(_)
    ));
}

#[test]
fn define_func() {
    let res = compile_and_run_with(
        r#"
    fn f(x, y) {
        x * y;
    }

    f(5, 5)
    "#,
        |vals| {
            assert_eq!(vals[0], Value::I64(25));
        },
    );
    // The return value does not matter
    assert!(res.is_ok());
}

#[test]
fn factorial() {
    let res = compile_and_run_with(
        r#"
fn fact(n: i64) -> i64 {
    if n < 1 {
        1
    } else {
        n * fact(n - 1)
    }
}

fact(10)
"#,
        |vals| assert_eq!(vals[0], Value::I64(3628800)),
    );
    assert!(res.is_ok());
}

#[test]
fn loop_test() {
    let res = compile_and_run_with(
        r#"var i = 0;
var accum = 0;

loop {
    i = i + 1;
    if 10 < i {
        break;
    };
    accum = accum + i;
}

print(accum);
"#,
        |vals| assert_eq!(vals[0], Value::I64(55)),
    );
    assert!(res.is_ok());
}

#[test]
fn for_test() {
    let res = compile_and_run_with(
        r#"var res = 0;
for i in 0 .. 10 {
    res = res + i;
}
print(res);
"#,
        |vals| assert_eq!(vals[0], Value::I64(45)),
    );
    assert!(res.is_ok());
}

#[test]
fn while_test() {
    let res = compile_and_run_with(
        r#"var i = 0;

while i < 10 {
    i = i + 1;
}

print(i);
"#,
        |vals| assert_eq!(vals[0], Value::I64(10)),
    );
    assert!(res.is_ok());
}

#[test]
fn array_init() {
    let res = compile_and_run_with("var a: [i32] = [1 + 3]; print(a); ", |vals| {
        assert_eq!(
            vals[0],
            Value::Array(ArrayInt::new(TypeDecl::Any, vec![Value::I64(4)]))
        )
    });
    assert!(res.is_ok());
}

#[test]
fn array_index() {
    let res = compile_and_run_with(
        r#"
var a = [1, 2, 3];
print(a[1]);"#,
        |vals| assert_eq!(vals[0], Value::I64(2)),
    );
    assert!(res.is_ok());
}

#[test]
fn array_assign_index() {
    let res = compile_and_run_with(
        r#"
var a = [1 + 3];
a[0] = 10;
print(a);"#,
        |vals| {
            assert_eq!(
                vals[0],
                Value::Array(ArrayInt::new(TypeDecl::Any, vec![Value::I64(10)]))
            )
        },
    );
    assert!(res.is_ok());
}

#[test]
fn chained_tuple_index() {
    let res = compile_and_run_with(
        r#"var a = (1, (2, 3), 4);
print(a.1.1);"#,
        |vals| assert_eq!(vals[0], Value::I64(3)),
    );
    assert!(res.is_ok());
}

#[test]
fn cmp() {
    for (op, expected) in [
        ("<=", 1),
        ("<", 0),
        (">=", 1),
        (">", 0),
        ("==", 1),
        ("!=", 0),
    ] {
        let s = format!("print(1 {} 1)", op);
        compile_and_run_with(&s, move |vals| assert_eq!(vals[0], Value::I32(expected))).unwrap();
    }
}
