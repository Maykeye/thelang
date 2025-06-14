use crate::AST;
use crate::ast::{ExprKind, Type};
use crate::tokens::Pos;
use crate::{cst::CST, lexer::tokenize};
use pretty_assertions::assert_eq;

use super::{AstError, ScopeTracker};

fn ast_from_text(source: &str) -> Result<AST, (AST, Vec<AstError>)> {
    let toks = tokenize(source).expect(&format!("Lexer failed for {source}"));
    let cst = CST::from_tokens(&toks).expect(&format!("CST failed for {source}"));
    AST::from_cst(cst)
}

fn make_empty_func_ast() -> AST {
    ast_from_text("\nfn empty_func()\n{\n}").unwrap()
}

#[test]
fn test_empty_func_decl() {
    let empty_func = &make_empty_func_ast().functions["empty_func"];
    assert_eq!(empty_func.name, "empty_func");
    assert_eq!(empty_func.r#type.return_type, Type::Unit);
    assert!(empty_func.get_args().is_empty());
}

#[test]
fn test_empty_func_decl_pos() {
    let empty_func = &make_empty_func_ast().functions["empty_func"];
    assert_eq!(empty_func.decl_pos, Pos::new(2, 1));
    let body = empty_func.body.as_ref().unwrap();
    assert_eq!(body.pos.line, 3);
    assert_eq!(body.pos.col, 1);
}

#[test]
fn test_empty_func_definition() {
    let empty_func = &make_empty_func_ast().functions["empty_func"];
    let body = empty_func.body.as_ref().unwrap();
    assert_eq!(body.return_type, Some(Type::Unit));
    assert_eq!(body.exprs.len(), 1);
    let ret_expr = match &body.exprs[0].kind {
        ExprKind::Return(ret_val) => ret_val.as_ref(),
        _ => {
            panic!("expected Return, not {:?}", body.exprs[0].kind)
        }
    };
    assert!(ret_expr.is_none());
}

fn impl_nesting_return_type(txt: &str) {
    let ast = ast_from_text(txt).unwrap();
    let body = ast.functions.get("nesting").unwrap().body.as_ref().unwrap();
    assert_eq!(body.return_type, Some(Type::Unit));
}
#[test]
fn test_nesting_return_type() {
    impl_nesting_return_type("fn nesting()\n{{}}");
    impl_nesting_return_type("fn nesting()\n{{return};}");
    impl_nesting_return_type("fn nesting()\n{{return}}");
    impl_nesting_return_type("fn nesting()\n{{{}}}");
    impl_nesting_return_type("fn nesting()\n{{};}");
    impl_nesting_return_type("fn nesting()\n{}");
}

#[test]
fn test_nesting_return_type_w_unit_type() {
    impl_nesting_return_type("fn nesting()\n{()}");
    impl_nesting_return_type("fn nesting()\n{{()};}");
    impl_nesting_return_type("fn nesting()\n{{()}}");
    impl_nesting_return_type("fn nesting()\n{{return ();};}");
    impl_nesting_return_type("fn nesting()\n{{return ()};}");
    impl_nesting_return_type("fn nesting()\n{{return ()}}");
    impl_nesting_return_type("fn nesting()\n{{{()}}}");
}

fn impl_test_args_ok(source: &str, n: usize) {
    let ast = ast_from_text(source).unwrap();
    let fun = ast.functions.get("fun").unwrap();
    assert_eq!(
        fun.get_args().len(),
        n,
        "{source} should have {n} args, not {}",
        fun.get_args().len()
    );
    for i in 0..n {
        let arg = &fun.get_args()[i];
        assert_eq!(arg.name, format!("a{i}"), "{source}: arg#{i} name mismatch");
    }
}

#[test]
fn test_args_ok() {
    impl_test_args_ok("fn fun(){}", 0);
    impl_test_args_ok("fn fun(a0:()){}", 1);
    impl_test_args_ok("fn fun(a0:(),){}", 1);
    impl_test_args_ok("fn fun(a0:(),a1:()){}", 2);
    impl_test_args_ok("fn fun(a0:(),a1:(),){}", 2);
    impl_test_args_ok("fn fun(a0:(),a1:(),a2:()){}", 3);
    impl_test_args_ok("fn fun(a0:(),a1:(),a2:(),){}", 3);
}

fn impl_test_args_err(source: &str) {
    let ast = ast_from_text(source);
    assert!(ast.is_err(), "source unexpectedly parsed: {source}");
}

#[test]
fn test_args_err() {
    impl_test_args_err("fn fun(a0:(),a0:()){}");
    impl_test_args_err("fn fun(a0:(),a1:(), a0:()){}");
}

fn impl_test_args_underscore(source: &str, is_underscore: &[bool]) {
    let ast = ast_from_text(source).unwrap();
    let fun = ast.functions.get("fun").unwrap();
    let n = is_underscore.len();
    assert_eq!(
        fun.get_args().len(),
        is_underscore.len(),
        "number of args mismatch\n{source}"
    );
    for i in 0..n {
        let arg = &fun.get_args()[i];
        if !is_underscore[i] {
            assert_eq!(arg.name, format!("a{i}"), "{source}: arg#{i} name mismatch");
        } else {
            assert_eq!(
                arg.name,
                format!("$arg${i}"),
                "{source}: arg#{i} underscore name mismatch"
            );
        }
    }
}

#[test]
fn test_args_underscore() {
    impl_test_args_underscore("fn fun(_:()){}", &[true]);
    impl_test_args_underscore("fn fun(_:(),_:()){}", &[true, true]);
    impl_test_args_underscore(
        "fn fun(a0:(), _:(), a2:(), _:()){}",
        &[false, true, false, true],
    );
}

#[test]
fn test_scoper() {
    let mut scope = ScopeTracker::new();

    let check = |scope: &ScopeTracker, value: &str| match scope.resolve_var("hello") {
        Some(x) => {
            let x = x;
            assert_eq!(x.as_str(), value, "{:?}", scope);
        }
        None => panic!("variable `hello` not found in {:?}", scope),
    };

    scope.push_scope();
    assert_eq!(scope.resolve_var("hello"), None);
    scope.declare_var("hello");
    check(&scope, "hello");
    scope.declare_var("hello");
    check(&scope, "hello$1");

    scope.push_scope();
    scope.declare_var("hello");
    check(&scope, "hello$2");
    scope.pop_scope();

    check(&scope, "hello$1");
    scope.declare_var("hello");
    check(&scope, "hello$3");
    scope.pop_scope();
}

#[test]
fn test_return_arg() {
    let ast = ast_from_text("fn main(ar1: (), ar2: ()){ar1;ar2}").unwrap();
    let func = ast.functions.get("main").unwrap();
    let body = func.body.as_ref().unwrap();
    assert_eq!(body.exprs.len(), 2);

    let expr = &body.exprs[0];
    let arg = match &expr.kind {
        ExprKind::Argument(name) => name,
        _ => panic!("Unexpected expr1 {expr:?}"),
    };
    assert_eq!(arg, "ar1");

    let expr = &body.exprs[1];
    let arg = match &expr.kind {
        ExprKind::Argument(name) => name,
        _ => panic!("Unexpected expr2 {expr:?}"),
    };
    assert_eq!(arg, "ar2");
}

#[test]
fn test_function_argument_type() {
    let ast = ast_from_text("fn tst(x: bool){}").unwrap();
    let func = ast.functions.get("tst").unwrap();
    assert_eq!(func.r#type.args.len(), 1);
    let arg = &func.r#type.args[0];
    assert_eq!(arg.r#type, Type::Bool);
}

#[test]
fn test_return_type_ok() {
    let ast = ast_from_text("fn truth()->bool{true}").unwrap();
    let func = ast.functions.get("truth").unwrap();
    assert_eq!(func.r#type.return_type, Type::Bool);
}
