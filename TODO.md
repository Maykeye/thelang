# Compiler Roadmap

## Immediate
* Add arguments
  * [x] added arguments tests
  * [x] fix gram1
  * [x] add cst test for `foo(,); foo(a b,,)` etc (not ast)
  * [x] fix cst, ast
  * [ ] add tests for ((({})))
  * [ ] add IROp for loading args
  * [ ] add IR type for arg
  * [ ] add IR regs names
  * [ ] stub in nasm64
* Merge cst/error_recovery_find_next_block_end/error_recovery_find_current_block_end/error_recovery_find_expr_end
* Write IR Types into output(?)
* Add (virtual) register allocation, scope = function(done). SSA + local mem. Have several kind of registers like R1(actual virtual reg), M1(memory), A1 where one is operated on, second stores data perhaps on stack third(A1) is argument.
* Add booleans type
* Add registers mappings
* Use enum error for error instead of string


## Delayed goals
* In error recovery, right now error_recovery_find_completed_block goes beyond RCurly. Fix so it stops at it(optionally?) as in body parsing we need RCurly to stop parsing. Also make error recovery that fast forward not to RCurly, but to Semi, as if expression is invalid, parsing next may be fine.
* Add lean4 backend
* Add *the* integer type (only `u64` for now )
* Add arguments
* Add comparison(at least `<`) for integer
* Add invariants (requires, ensures, invariant, etc)
* Produce invariants to Lean4 for formal proof of our language programs.
* Local variables
* Add AST type "Any" to which anything can be assiigned, so if function/var type is not parsed, we can continue parsing the rest
## Much later goals
* overshadowing local variables (`let a = 1; let a = 1`); document that '_' has internal name and using r#$arg$1 will overshadow it
* Other types
* Memory allocation
* Generics
* Modules where function names can "conflict" with function names with other modules (eg `foo::a` and `boo::a`)

## Much much much later
* We need syntax for destructive binding for `let` eg `let mut i=10; let (existing i, new j) = (10, 20)`
* Harmonize IR::to_text(->String) with codegen::to_text(->vec<string>)

