use mascal::*;
use nom::Finish;
use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

#[test]
fn test_default_arg() {
    let src = r#"
fn add(a: i32 = 123, b: i32 = 456) -> i32 {
    a + b
}

output(add());
"#;
    parse_compile_interpret(src, b"579");
}

/// Tests the function default arguments with constant expression
#[test]
fn test_default_arg_const_expr() {
    let src = r#"
fn double(a: i32 = 1 + 2 + 3) -> i32 {
    a * 2
}

output(double());
"#;
    parse_compile_interpret(src, b"12");
}

fn parse_compile_interpret(src: &str, expected: &[u8]) {
    let (_, ast) = source(src).finish().unwrap();
    let buf = Rc::new(RefCell::new(vec![]));
    let output = s_writer(buf.clone());
    let mut funcs = HashMap::new();
    funcs.insert("output".to_string(), output);
    let bytecode = compile(&ast, funcs).unwrap();
    interpret(&bytecode).unwrap();
    assert_eq!(*buf.borrow(), expected);
}

/// Take an output mutable shared buffer behind Rc-RefCell, returns a closure that writes to it.
fn s_writer(out: Rc<RefCell<Vec<u8>>>) -> Box<dyn Fn(&[Value]) -> Result<Value, EvalError>> {
    Box::new(move |vals| {
        let mut writer = out.borrow_mut();
        for val in vals {
            write!(writer, "{}", val).unwrap();
        }
        Ok(Value::I32(0))
    })
}
