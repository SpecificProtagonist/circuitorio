**This project is very much WIP**  
Circuitorio is a DSL for designing circuit networks in factorio. 

The language resticts itself to combinators and makes no attempts to integrate othe entities that can interact with circuit networks. It also is rather low level, more akin to a macro assembler than a high level compiler.

## Features
- model combinators via DSL
- simulation
- **not yet implemented:** export to blueprint string

## Usage
no  
This doesn't have a CLI or acceptable API yet  
TODO: add instructions when this is at least somewhat usable  
To run, install [Rust](https://www.rust-lang.org/) and run `cargo run`

## Language
The language does not use significant whitespace.  
Line comments are introduced with `#`:

    # This is a comment

A network `foo` can be declared with

    net(red) foo [some_signal some_other_signal]

A network is either `red` or `green`. Signal names are arbitrary and shared between networks. They will be mapped to Factorio signals at export time. Combinators can only access signals mentioned in the relevant network declarations.  
A constant combinator can be declared with

    foo[some_signal] <- 5

Multiple constant combinators connected to the same networks will be combined internally. A combinator can be connected to a `red` and a `green` network simulaneously:

    (foo bar)[some_signal] <- 5

Arithmetic combinators:

    foo[some_signal] <- (foo bar)[signal * 3]

The operators are `+`, `-`, `*`, `/`, `%`, `**`, `<<`, `>>`, `&`, `|` and `^`.  The right hand side can be a concrete signal or a value. The output and left input can be a concrete signal or `each`. If the operation should do nothing, it can be left out:

    memory[x] <- memory[x]

Decider combinator:

    foo[some_signal] <- one if foo[trigger > 0]
    bar[every] <- value if foo[every <= some_signal]

The operators are `>`, `<`, `==`, `>=`, `<=`, and `!=`. Output and left input can be a concrete signal, `each`, `every` or `any`. `one` and `value` decides between 1 and input count output modes.

Arithmetic expressions (brackets required for nesting) can be used as values. These can also include parameters, which are declared with `=`:

    mask = (1 << 16) - 1

To repeat a set of combinators, use `loop`:

    loop address from 0 to number_of_registers {
        net(green) register[memory]
        read[memory] <- value if (io register)[read_addr == address]
        # more stuff here plz
    }

This creates the body once for every loop iteration. Lower bound is inclusive, upper bound exclusive. All declarations made in the loop body are only in scope within the body.

A new scope can also be introduced using `{` `}`

(todo: submodules/top level statements)  
To reuse code, you can declare a module:

    module blah<param other_param> (
        net(red) thingies[x, y]
        net(red) doodah[x]
    ) {
        # Statements go here
    }

and instantiate it:

    blah<32 4>(
        foo[x: some_signal, y: some_other_signal]
        bar[x: some_signal]
    )

If the signal names match, you can use `x` instead of `x: x`.

To indicate the direction data flows and prevent mistakes you can optionally add `(in)` or `(out)` after a signal in the module argument declaration. This is specific to the network-signal combo.

Planned: signal arrays, if statements

## Syntax highlighting
If you use VS Code, you can copy the `code-extension` folder into you extensions folder to gain shitty syntax highlighting (triggered by the `.fhdl` file extension)

## Awesome similiar projects
todo: comparison  
[Verilog2Factorio](https://github.com/Redcrafter/verilog2factorio)  
[CombinatorC](https://github.com/osimon8/CombinatorC)