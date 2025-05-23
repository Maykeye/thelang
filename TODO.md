# Compiler Roadmap

## Immediate

* Break `ast::test_ast::test_nesting_return_type_w_unit_type`
* {{}} in ir should be `r1 = call b2`, in nasm it should be `call .b2`
* Add (virtual) register allocation, scope = function. SSA + local mem. Have several kind of registers like R1(actual virtual reg), M1(memory), A1 where one is operated on, second stores data perhaps on stack third(A1) is argument.


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


### DONE
* Implement IR test for nesting code blocks (copy-paste main.rs is enough)
