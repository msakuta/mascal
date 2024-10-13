use std::collections::HashMap;

use mascal::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    pub(crate) fn log(s: &str);
}

#[wasm_bindgen(module = "/wasm_api.js")]
extern "C" {
    pub(crate) fn wasm_print(s: &str);
    pub(crate) fn wasm_rectangle(x0: i32, y0: i32, x1: i32, y1: i32);
    pub(crate) fn wasm_set_fill_style(s: &str);
}

fn s_print(vals: &[Value]) -> Result<Value, EvalError> {
    wasm_print("print:");
    let output = vals
        .iter()
        .map(|v| v.to_string())
        .fold(String::new(), |acc, cur| {
            if acc.is_empty() {
                cur
            } else {
                acc + ", " + &cur
            }
        });
    wasm_print(&output);

    wasm_print(&format!("\n"));
    Ok(Value::I32(0))
}

fn s_puts(vals: &[Value]) -> Result<Value, EvalError> {
    fn puts_inner(vals: &[Value]) {
        for val in vals {
            match val {
                Value::F64(val) => wasm_print(&format!("{}", val)),
                Value::F32(val) => wasm_print(&format!("{}", val)),
                Value::I64(val) => wasm_print(&format!("{}", val)),
                Value::I32(val) => wasm_print(&format!("{}", val)),
                Value::Str(val) => wasm_print(&format!("{}", val)),
                Value::Array(val) => puts_inner(
                    &val.borrow()
                        .values()
                        .iter()
                        .map(|v| v.clone())
                        .collect::<Vec<_>>(),
                ),
                Value::Tuple(val) => puts_inner(
                    &val.borrow()
                        .iter()
                        .map(|v| v.value().clone())
                        .collect::<Vec<_>>(),
                ),
            }
        }
    }
    puts_inner(vals);
    Ok(Value::I32(0))
}

fn s_rectangle(vals: &[Value]) -> Result<Value, EvalError> {
    let mut i32vals = vals.iter().take(4).map(|val| {
        if let Ok(Value::I32(v)) = coerce_type(val, &TypeDecl::I32) {
            Ok(v)
        } else {
            Err("wrong type!".to_string())
        }
    });
    let short = || "Input needs to be more than 4 values".to_string();
    let x0 = i32vals.next().ok_or_else(short)??;
    let y0 = i32vals.next().ok_or_else(short)??;
    let x1 = i32vals.next().ok_or_else(short)??;
    let y1 = i32vals.next().ok_or_else(short)??;
    wasm_rectangle(x0, y0, x1, y1);
    Ok(Value::I32(0))
}

fn s_set_fill_style(vals: &[Value]) -> Result<Value, EvalError> {
    if let [Value::Str(s), ..] = vals {
        wasm_set_fill_style(s);
    }
    Ok(Value::I32(0))
}

fn wasm_functions<'src, 'native>(mut set_fn: impl FnMut(&'static str, FuncDef<'src, 'native>)) {
    set_fn("print", FuncDef::new_native(&s_print, vec![], None));
    set_fn(
        "puts",
        FuncDef::new_native(&s_puts, vec![ArgDecl::new("val", TypeDecl::Any)], None),
    );
    set_fn(
        "set_fill_style",
        FuncDef::new_native(
            &s_set_fill_style,
            vec![ArgDecl::new("style", TypeDecl::Str)],
            None,
        ),
    );
    set_fn(
        "rectangle",
        FuncDef::new_native(
            &s_rectangle,
            vec![
                ArgDecl::new("x0", TypeDecl::I64),
                ArgDecl::new("y0", TypeDecl::I64),
                ArgDecl::new("x1", TypeDecl::I64),
                ArgDecl::new("y1", TypeDecl::I64),
            ],
            None,
        ),
    );
}

#[wasm_bindgen]
pub fn type_check(src: &str) -> Result<JsValue, JsValue> {
    let mut ctx = TypeCheckContext::new(None);
    wasm_functions(|name, f| ctx.set_fn(name, f));
    let parse_result =
        source(src).map_err(|e| JsValue::from_str(&format!("Parse error: {:?}", e)))?;
    mascal::type_check(&parse_result.1, &mut ctx)
        .map_err(|e| JsValue::from_str(&format!("Error on type check: {}", e)))?;
    Ok(JsValue::from_str("Ok"))
}

#[wasm_bindgen]
pub fn run_script(src: &str) -> Result<(), JsValue> {
    let mut ctx = EvalContext::new();
    wasm_functions(|name, f| ctx.set_fn(name, f));
    let parse_result =
        source(src).map_err(|e| JsValue::from_str(&format!("Parse error: {:?}", e)))?;
    if 0 < parse_result.0.len() {
        return Err(JsValue::from_str(&format!(
            "Unexpected end of input at: {:?}",
            parse_result.0
        )));
    }
    run(&parse_result.1, &mut ctx)
        .map_err(|e| JsValue::from_str(&format!("Error on execution: {:?}", e)))?;
    Ok(())
}

#[wasm_bindgen]
pub fn parse_ast(src: &str) -> Result<String, JsValue> {
    let parse_result =
        source(src).map_err(|e| JsValue::from_str(&format!("Parse error: {:?}", e)))?;
    Ok(format!("{:#?}", parse_result.1))
}

#[wasm_bindgen]
pub fn compile(src: &str) -> Result<Vec<u8>, JsValue> {
    let parse_result =
        source(src).map_err(|e| JsValue::from_str(&format!("Parse error: {:?}", e)))?;
    let mut functions = HashMap::new();
    extra_functions(&mut |name, f| {
        functions.insert(name, f);
    });
    let bytecode = mascal::compile(&parse_result.1, functions)
        .map_err(|e| JsValue::from_str(&format!("Error: {}", e)))?;
    let mut bytes = vec![];
    bytecode
        .write(&mut bytes)
        .map_err(|s| JsValue::from_str(&s.to_string()))?;
    Ok(bytes)
}

#[wasm_bindgen]
pub fn disasm(src: &str) -> Result<String, JsValue> {
    let parse_result =
        source(src).map_err(|e| JsValue::from_str(&format!("Parse error: {:?}", e)))?;
    let mut functions = HashMap::new();
    extra_functions(&mut |name, f| {
        functions.insert(name, f);
    });
    let disasm_code = mascal::disasm(&parse_result.1, functions)
        .map_err(|e| JsValue::from_str(&format!("Error: {}", e)))?;
    Ok(disasm_code)
}

#[wasm_bindgen]
pub fn compile_and_run(src: &str) -> Result<(), JsValue> {
    let parse_result =
        source(src).map_err(|e| JsValue::from_str(&format!("Parse error: {:?}", e)))?;
    let mut functions = HashMap::new();
    extra_functions(&mut |name, f| {
        functions.insert(name, f);
    });
    let mut bytecode = mascal::compile(&parse_result.1, functions)
        .map_err(|e| JsValue::from_str(&format!("Error: {}", e)))?;
    bytecode.add_std_fn();
    bytecode.cache_bytecode();
    extra_functions(&mut |name, f| {
        bytecode.add_ext_fn(name, f);
    });
    interpret(&bytecode).map_err(|e| JsValue::from_str(&e.to_string()))?;
    Ok(())
}

fn extra_functions(f: &mut impl FnMut(String, Box<dyn Fn(&[Value]) -> Result<Value, EvalError>>)) {
    f("print".to_string(), Box::new(s_print));
    f("puts".to_string(), Box::new(s_puts));
    f("set_fill_style".to_string(), Box::new(s_set_fill_style));
    f("rectangle".to_string(), Box::new(s_rectangle));
}
