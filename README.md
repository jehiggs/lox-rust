# Lox Rust

A Rust implementation of the second half of [Crafting Interpreters](https://craftinginterpreters.com/a-bytecode-virtual-machine.html).

The second half of the book focuses on building a bytecode virtual machine that matches the functionality of the tree walk interpreter in the first half.

The aim in this repository was to both learn how to do this and to (re-)learn how to write Rust programs. There are some interesting implementation choices in here as a result! Once this project is complete I will try and sketch out some of what I have learned in doing this.

Currently this is only complete up to and including the chapter on Functions (24).

## Running the project

You can run the REPL using Cargo:

```sh
cargo run --release
```

Alternatively you can execute one of the example programs as follows:

```sh
cargo run --release -- examples/<lox-file>
```

Note that running without the release flag will print a potentially very large amount of debug information!
