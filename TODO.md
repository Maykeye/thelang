# Compiler Roadmap

## Immediate
* Refactor Errors to Enum
 * [!] Lexer
 * [x] AST
 * [X] CST
 * [ ] IR
 * [ ] NASM64
* [ ] Implement bool:and 
* [ ] make and(x,y) -> x & b func
* Add booleans type
 * [x] focus on test_load_bool in codegen_nasm64
* Add arguments
  * [ ] several args 
  * [ ] x64: load several args via regs(r15-r10? copy sysv?)
  * [ ] x64: others via stack
* Write IR Types into output(?)
* Add (virtual) register allocation, scope = function(done). 
   * [ ] combine kinds of registers: one operational SSA + one local mem for loading/storing + hardware (for rax, rbx, etc)
* Add registers mappings
* Merge cst/error_recovery_find_next_block_end/error_recovery_find_current_block_end/error_recovery_find_expr_end


## Delayed goals
* Cleanup CST parsing.
* CST Expr tests: make tests for each grammar rule, where possible, where grammar is incorrect.  Each grammar rule, if broken, should recover, not loop eternally
* Add lean4 backend
* scopetrackerhandler with Drop to push/pop scope
* Add *the* integer type (only `u64` for now )
* Add comparison(at least `<`) for integer
* Add invariants (requires, ensures, invariant, etc)
* Produce invariants to Lean4 for formal proof of our language programs.
* Local variables
* AST shoudld have NodeKind::Argument(argument_num) instead of argument_name; so var_data.is_arg should be replaced with arg:Option<int>
* Add AST type "Any" to which anything can be assiigned, so if function/var type is not parsed, we can continue parsing the rest

## Much later goals
* AST should have TypeId instead of cloning type on every step
* Other types
* Memory allocation
* Generics
* Modules where function names can "conflict" with function names with other modules (eg `foo::a` and `boo::a`)

## Much much much later
* We need syntax for destructive binding for `let` eg `let mut i=10; let (existing i, new j) = (10, 20)`
* Harmonize IR::to_text(->String) with codegen::to_text(->vec<string>)
* Rename CST to something more correct: TLAST(TypelessAST/BaseTree or something)
