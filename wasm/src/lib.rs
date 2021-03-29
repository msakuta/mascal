use parser::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    pub(crate) fn log(s: &str);
}

#[wasm_bindgen(module = "/wasm_api.js")]
extern "C" {
    pub(crate) fn wasm_print(s: &str);
}

fn s_print(vals: &[Value]) -> Value {
    wasm_print("print:");
    fn print_inner(vals: &[Value]) {
        for val in vals {
            match val {
                Value::F64(val) => wasm_print(&format!(" {}", val)),
                Value::F32(val) => wasm_print(&format!(" {}", val)),
                Value::I64(val) => wasm_print(&format!(" {}", val)),
                Value::I32(val) => wasm_print(&format!(" {}", val)),
                Value::Str(val) => wasm_print(&format!(" {}", val)),
                Value::Array(_, val) => {
                    wasm_print("[");
                    print_inner(&val.iter().map(|v| v.borrow().clone()).collect::<Vec<_>>());
                    wasm_print("]");
                }
                Value::Ref(r) => {
                    wasm_print("ref(");
                    print_inner(&[r.borrow().clone()]);
                    wasm_print(")");
                }
            }
        }
    }
    print_inner(vals);
    wasm_print(&format!("\n"));
    Value::I32(0)
}

fn s_puts(vals: &[Value]) -> Value {
    fn puts_inner(vals: &[Value]) {
        for val in vals {
            match val {
                Value::F64(val) => wasm_print(&format!("{}", val)),
                Value::F32(val) => wasm_print(&format!("{}", val)),
                Value::I64(val) => wasm_print(&format!("{}", val)),
                Value::I32(val) => wasm_print(&format!("{}", val)),
                Value::Str(val) => wasm_print(&format!("{}", val)),
                Value::Array(_, val) => {
                    puts_inner(&val.iter().map(|v| v.borrow().clone()).collect::<Vec<_>>())
                }
                Value::Ref(r) => puts_inner(&[r.borrow().clone()]),
            }
        }
    }
    puts_inner(vals);
    Value::I32(0)
}

#[wasm_bindgen]
pub fn entry(src: &str) -> Result<(), JsValue> {
    let mut ctx = EvalContext::new();
    ctx.set_fn("print", FuncDef::Native(&s_print));
    ctx.set_fn("puts", FuncDef::Native(&s_puts));
    run(&source(src).unwrap().1, &mut ctx)
        .map_err(|e| JsValue::from_str(&format!("Error on execution: {:?}", e)))?;
    Ok(())
}