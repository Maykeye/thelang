# Compiler Roadmap

## Immediate
* Add arguments
  * [x] added arguments tests
  * [x] fix gram1
  * [x] add cst test for `foo(,); foo(a b,,)` etc (not ast)
  * [x] fix cst, ast
  * [ ] add IROp for loading args
  * [ ] add IR type for arg
  * [ ] add IR regs names
  * [ ] stub in nasm64
* Merge cst/error_recovery_find_next_block_end/error_recovery_find_current_block_end/error_recovery_find_expr_end
* Write IR Types into output(?)
* Add (virtual) register allocation, scope = function(done). 
   * [ ] combine kinds of registers: one operational SSA + one local mem for loading/storing.
* Add booleans type
* Add registers mappings
* Use enum error for error instead of string


## Delayed goals
* CST Expr tests: make tests for each grammar rule, where possible, where grammar is incorrect.  Each grammar rule, if broken, should recover, not loop eternally
* Add lean4 backend
* Add *the* integer type (only `u64` for now )
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

