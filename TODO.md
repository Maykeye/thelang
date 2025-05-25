# Compiler Roadmap

## Immediate
* Add arguments
  * [x] added arguments tests
  * [x] fix gram1
  * [x] add cst test for `foo(,); foo(a b,,)` etc (not ast)
  * [ ] fix cst
  * [ ] fix ast
  * [ ] stub in nasm64
* Write IR Types into output(?)
* Add (virtual) register allocation, scope = function(done). SSA + local mem. Have several kind of registers like R1(actual virtual reg), M1(memory), A1 where one is operated on, second stores data perhaps on stack third(A1) is argument.
* Add booleans type
* Add registers mappings


## Delayed goals
* Add lean4 backend
* Add *the* integer type (only `u64` for now )
* Add arguments
* Add comparison(at least `<`) for integer
* Add invariants (requires, ensures, invariant, etc)
* Produce invariants to Lean4 for formal proof of our language programs.
* Local variables

## Much later goals
* overshadowing local variables (`let a = 1; let a = 1`)
* Other types
* Memory allocation
* Generics
* Modules where function names can "conflict" with function names with other modules (eg `foo::a` and `boo::a`)

## Much much much later
* We need syntax for destructive binding for `let` eg `let mut i=10; let (existing i, new j) = (10, 20)`
* Harmonize IR::to_text(->String) with codegen::to_text(->vec<string>)

