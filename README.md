# Mascal

MASsive CALculator

An experimental language for large scale array manipulation, similar to numpy or Matlab, in Rust.

Try it now on your browser! https://msakuta.github.io/mascal/

## Requirements

* rust 1.44
* npm 7.0.2 (for WebAssembly browser, any web server should work)


# How to build

There are 2 ways to build this project.

* Command line interpreter
* WebAssembly browser application

## Command line interpreter

One is for native command line application.
It can read a text file, parse it to an AST and run it.

    cd cli
    cargo run --release -- <script-file>.dragon

You can type check the script before running with `-t` switch.
It will ensure that the declared types are correct before running the script.
The language itself is still dynamically typed, but it will help writing robust software.

    cargo run --release -- -t

We have our own bytecode format that you can compile.

    cargo run --release -- -c <script-file>.dragon

It will create an output file "out.cdragon" which is a pre-compiled bytecode
that can run faster than AST interpreter.
It is similart to ".pyc" file against ".py" in Python.

You can also compile and run at the same time.

    cargo run --release -- -cR <script-file>.dragon

If you have a pre-compiled bytecode file, you can just run it without compiling as:

    cargo run --release -- -b <bytecode>.cdragon


## Interactive debugger

The command line interpreter has an interactive debugger, in which you can run a script by steps.

![](images/screenshot00.png)

You can enter this interactive debugger mode by specifying `-D` flag to the CLI.

    cd cli
    cargo run --release -- -bD <bytecode>.cdragon


## WebAssembly browser application

You can also build a wasm package and run the interpreter on the browser.

    cd wasm
    npm run build

To launch the application, you can use `npx`

    cd dist
    npx serve

and browse http://localhost:5000.

## TODOs

In ascending order of difficulty.

* [x] Arithmetic operators (+, -, *, /)
* [x] Functions, recursive calls
* [x] Loops (for, while, loop)
* [x] Proper expression statements (brace expressions)
* [x] Variable definition initializer
* [x] Type declaration
* [x] Primitive types (i32, i64, f32, f64)
* [x] String type
* [x] Logical operators (||, &&, !)
* [x] Bitwise operators (|, &, ^, ~)
* [ ] String manipulations
* [x] Array types
* [x] WebAssembly build target
* [x] Function return types
* [x] Static type checking (instead of runtime coercion)
* [x] Type cast operator `as`
* [x] Line and block comments (`/*`, `*/`, `//`)
* [x] Comparison operators (`<`, `>`, `<=`, `>=`, `==`, `!=`)
* [x] Pipe operator (`|>`), which chains functions with arity 1. Note that there is no currying.
* [x] [Named arguments in function calls](https://github.com/msakuta/mascal/wiki/Function-signature#named-argument-in-function-call)
* [x] [Default argument](https://github.com/msakuta/mascal/wiki/Function-signature#default-argument)
* [x] Type casting in bytecode
* [x] Proper error handling
* [x] Tuple types
* [x] Type inference
* [x] Structs (memory-packed container with named fields)
* [ ] Multi-dimensional arrays
* [x] Function types (first class function variables)
* [ ] Lambda expressions
* [ ] Mutability qualifiers
* [ ] Array slice syntax
* [ ] Array shape constraints
* [ ] Broadcasting operators
* [ ] Custom operators
* [x] Run on VM (not directly on AST)
* [x] Compile to bytecode

## Ideas

I want to make it a complementary DSL for data manipulation, such as numpy.

* First-class array and matrix operations - from Matlab
* Statically typed array shapes - from Futhark
* Organized broadcasting operators (dot prefix) - from Julia
* Adapt to LLVM backend to make it a native compiler
