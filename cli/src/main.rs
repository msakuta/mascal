mod debugger;

use clap::Parser;
use debugger::run_debugger;
use mascal::*;

use ::colored::Colorize;
use std::{
    collections::HashMap,
    fs::File,
    io::{prelude::*, BufReader, BufWriter},
};

#[derive(Parser, Debug)]
#[clap(author, version, about = "A CLI interpreter of dragon language")]
struct Args {
    #[clap(help = "Input source file name or one-linear program")]
    input: String,
    #[clap(short, long, help = "Evaluate one line program")]
    eval: bool,
    #[clap(short, long, help = "Show AST")]
    ast: bool,
    #[clap(short = 'A', long, help = "Show AST in pretty format")]
    ast_pretty: bool,
    #[clap(short = 't', long, help = "Type check AST")]
    type_check: bool,
    #[clap(short, help = "Compile to bytecode")]
    compile: bool,
    #[clap(short = 'R', help = "Compile and run")]
    compile_and_run: bool,
    #[clap(short, help = "Read from bytecode")]
    bytecode: bool,
    #[clap(
        short,
        long,
        help = "Show disassembly of compiled or read bytecode, given -b, -c or -R was specified"
    )]
    disasm: bool,
    #[clap(short, long, help = "Show signatures of functions")]
    signatures: bool,
    #[clap(short = 'D', long, help = "Enable interactive debugger")]
    debugger: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let parse_source = |code, source_file| -> Result<(), Box<dyn std::error::Error>> {
        let result = source(code).map_err(|e| format!("{:#?}", e))?;
        if !result.0.is_empty() {
            return Err(format!("Input has terminated unexpectedly: {:#?}", result.0).into());
        }
        if args.ast_pretty {
            println!("Match: {:#?}", result.1);
        } else if args.ast {
            println!("Match: {:?}", result.1);
        }
        if args.type_check {
            if let Err(e) = type_check(&result.1, &mut TypeCheckContext::new(source_file)) {
                eprintln!("Type check error: {}", e.to_string().red());
                return Ok(());
            }
        }

        if args.compile || args.compile_and_run {
            let mut bytecode = compile(&result.1, HashMap::new())
                .map_err(|e| format!("Error: {}:{}", source_file.unwrap_or("<Unknown>"), e))?;
            if args.signatures {
                bytecode
                    .signatures(&mut std::io::stdout())
                    .map_err(|e| e.to_string())?;
            }
            if args.disasm {
                bytecode
                    .disasm(&mut std::io::stdout())
                    .map_err(|e| e.to_string())?;
            }
            if let Ok(writer) = std::fs::File::create("out.cdragon") {
                bytecode
                    .write(&mut BufWriter::new(writer))
                    .map_err(|s| s.to_string())?;
            }
            if args.compile_and_run {
                bytecode.add_std_fn();
                if args.debugger {
                    run_debugger(&bytecode)?;
                } else {
                    interpret(&bytecode)?;
                }
            }
        } else {
            run(&result.1, &mut EvalContext::new())
                .map_err(|e| format!("Error in run(): {}", e))?;
        }

        Ok(())
    };

    if args.eval {
        parse_source(&args.input, None)?;
    } else if let Ok(mut file) = File::open(&args.input) {
        if args.bytecode {
            let mut bytecode =
                Bytecode::read(&mut BufReader::new(file)).map_err(|e| e.to_string())?;
            if args.signatures {
                bytecode
                    .signatures(&mut std::io::stdout())
                    .map_err(|e| e.to_string())?;
            }
            if args.disasm {
                bytecode
                    .disasm(&mut std::io::stdout())
                    .map_err(|e| e.to_string())?;
            }
            bytecode.add_std_fn();
            interpret(&bytecode).map_err(|e| e.to_string())?;
        } else {
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|e| e.to_string())?;
            if let Err(e) = parse_source(&contents, Some(&args.input)) {
                println!("{e}");
            };
        }
    } else {
        return Err("Error: can't open file".into());
    }

    Ok(())
}
