# Thelan programming language 

Version 0.0.-∞

(I haven't even decided *the lan*guage name yet).
Very slowly developing a programming language, using version control for backups.
The goal is a mystery. Documentation is this file to not forget completely.

## Design:

A mini compiler works in stages. 

* The first stage is lexer(`src/lexer.rs`) -> CST (`src/cst.rs`).

It takes the pure text and produces CST - concrete syntax tree. As concrete to the source as possible.
It basically checks for syntax errors without caring for logic at all.

* The second stage is CST(`src/cst.rs`) -> AST(`src/ast.rs`).

AST takes CST and parses concrete syntax tree into abstract. Its idea is to parse what actually is going on: if function `f` is mentioned, what `f` actually is.

Integers? Pointers? Whodat.

## Current state:

It can create nasm64 linux assembler code for absolutely empty functions

`fn main(){}`

## Syntax

* FUNCTION_DEFINITION::= `fn` IDENTIFIER `(` ARGS `)` `{` CODE_BLOCK `}`

* ARGS ::= [IDENTIFIER `:` TYPE {`,` IDENTIFIER `:` TYPE} [`,`]]

* CODE_BLOCK ::= {`;`} [EXPR {`;` {`;`} EXPR} {`;`}]

* EXPR ::= EXPR_BINARY 

* EXPR_UNARY ::= `return` [EXPR] (*type: !*) 
             | `!` EXPR_TERM
             | EXPR_TERM

* EXPR_BINARY ::= EXPR_UNARY { `&` EXPR_UNARY }

* EXPR_TERM ::= `()` (*type unit*) 
   | `{` CODE_BLOCK `}`
   | IDENTIFIER
   | `(` EXPR `)`

* IDENTIFIER ::=  ( alpha | `_` ) { alpha | digit | `_` | `$` }

* TYPE ::= IDENTIFIER | `()`

* alpha ::= char.is_alphabetic (*per rust api*)

* digit ::= char.is_numeric (*per rust api*)
