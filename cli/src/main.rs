use clap::{crate_authors, crate_version, App, Arg};
use parser::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() -> Result<(), String> {
    let matches = App::new("rusty-parser")
        .version(crate_version!())
        .author(crate_authors!())
        .about("A CLI interpreter of dragon language")
        .arg(Arg::from_usage(
            "<INPUT>, 'Input source file name or one-linear program'",
        ))
        .arg(Arg::from_usage("-e, 'Evaluate one line program'"))
        .get_matches();

    let mut contents = String::new();
    let code = if let Some(file) = matches.value_of("INPUT") {
        if 0 < matches.occurrences_of("e") {
            file
        } else if let Ok(mut file) = File::open(&file) {
            file.read_to_string(&mut contents)
                .map_err(|e| e.to_string())?;
            &contents
        } else {
            return Err("Error: can't open file".to_string());
        }
    } else {
        return Ok(());
    };
    if let Ok(result) = source(code) {
        if 0 < result.0.len() {
            return Err(format!("Input has terminated unexpectedly: {:?}", result.0));
        }
        println!("Match: {:?}", result.1);
        run(&result.1, &mut EvalContext::new()).expect("Error in run()");
    } else {
        println!("failed");
    }
    Ok(())
}
