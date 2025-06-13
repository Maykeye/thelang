use crate::AST;
use crate::ast::ExprKind;
use crate::unwrap_variant;
use crate::{cst::CST, lexer::tokenize};

fn ast_from_text(source: &str) -> Result<AST, (AST, Vec<String>)> {
    let toks = tokenize(source).expect(&format!("Lexer failed for {source}"));
    let cst = CST::from_tokens(&toks).expect(&format!("CST failed for {source}"));
    AST::from_cst(cst)
}

#[test]
fn test_invert_ok() {
    let ast = ast_from_text("fn inv(x: bool) -> bool{!x}").unwrap();
    let func = ast.functions.get("inv").unwrap();
    let body = func.body.as_ref().unwrap();
    assert_eq!(body.exprs.len(), 1);
    let inner = unwrap_variant!(&body.exprs[0].kind, ExprKind::Invert);
    let ident = unwrap_variant!(&inner.kind, ExprKind::Argument);
    assert_eq!(ident, "x")
}

#[test]
fn test_invert_is_not_unit_type() {
    let ast = ast_from_text("fn inv(x: bool) {\n!x}");
    let err = ast.unwrap_err().1;
    assert_eq!(
        err[0],
        "2:1: type mismatch for function return value: () expected, got bool"
    );
    assert_eq!(err.len(), 1);
}

#[test]
fn test_invert_requires_bool() {
    let ast = ast_from_text("fn inv(x: ()){\n!x}");
    let err = ast.unwrap_err().1;
    assert_eq!(err[0], "2:1: type mismatch for !: bool expected, got ()");
    assert_eq!(err.len(), 1);
}
