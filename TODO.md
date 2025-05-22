# Compiler Roadmap

## Immediate

* Fix `ast::test_ast::test_nesting_return_type`

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
* fix `;` to be a separator and not a unit expression
* Other types
* Memory allocation
* Generics
* Modules where function names can "conflict" with function names with other modules (eg `foo::a` and `boo::a`)
