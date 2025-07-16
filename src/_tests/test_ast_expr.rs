use crate::AST;
use crate::ast::{AstErrorContextKind, AstTypeId, ExprKind};
use crate::tokens::{Pos, TokenKind};
use crate::unwrap_variant;
use crate::{cst::CST, lexer::tokenize};
use pretty_assertions::assert_eq;

use super::AstError;

fn ast_from_text(source: &str) -> Result<AST, (AST, Vec<AstError>)> {
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
fn test_and_ok() {
    let ast = ast_from_text("fn and_ok(x: bool, y:bool) -> bool{x & y}").unwrap();
    let func = ast.functions.get("and_ok").unwrap();
    let body = func.body.as_ref().unwrap();
    assert_eq!(body.exprs.len(), 1);
    let (lhs, rhs) = unwrap_variant!(&body.exprs[0].kind, ExprKind::And, 2);
    let lhs = unwrap_variant!(&lhs.kind, ExprKind::Argument);
    assert_eq!(lhs, "x");
    let rhs = unwrap_variant!(&rhs.kind, ExprKind::Argument);
    assert_eq!(rhs, "y");
}

#[test]
fn test_and_with_unary() {
    let ast = ast_from_text("fn and_with_unary_args(x: bool, y:bool) -> bool{!x & !y}").unwrap();
    let func = ast.functions.get("and_with_unary_args").unwrap();
    let body = func.body.as_ref().unwrap();
    assert_eq!(body.exprs.len(), 1);
    let (lhs, rhs) = unwrap_variant!(&body.exprs[0].kind, ExprKind::And, 2);
    let lhs = unwrap_variant!(&lhs.kind, ExprKind::Invert);
    let rhs = unwrap_variant!(&rhs.kind, ExprKind::Invert);
    let lhs = unwrap_variant!(&lhs.kind, ExprKind::Argument);
    assert_eq!(lhs, "x");
    let rhs = unwrap_variant!(&rhs.kind, ExprKind::Argument);
    assert_eq!(rhs, "y");
}

#[test]
fn test_and_requires_bool() {
    fn test(expr: &str, err_pos: Pos) {
        let source = "fn and(u: (), b: bool)->bool{".to_string();
        let source = source + expr + "\n}";
        let ast = ast_from_text(&source);
        let err = ast.unwrap_err().1;

        assert_eq!(err.len(), 1);
        let (ctx, from, to) = unwrap_variant!(&err[0], AstError::TypeConversion, 3);
        assert_eq!(ctx.error_pos, err_pos);
        assert_eq!(ctx.kind, AstErrorContextKind::BinOp(TokenKind::Ampersand));
        assert_eq!(from, &AstTypeId::UNIT);
        assert_eq!(to, &AstTypeId::BOOL);
    }
    test("u&b", Pos::new(2, 1));
    test("b&u", Pos::new(2, 3));
}

#[test]
fn test_invert_is_not_unit_type() {
    let ast = ast_from_text("fn inv(x: bool) {\n!x}");
    let err = ast.unwrap_err().1;

    assert_eq!(err.len(), 1);
    let (ctx, from, to) = unwrap_variant!(&err[0], AstError::TypeConversion, 3);
    assert_eq!(ctx.error_pos, Pos::new(2, 1));
    assert_eq!(ctx.kind, AstErrorContextKind::FunctionReturn);
    assert_eq!(from, &AstTypeId::BOOL);
    assert_eq!(to, &AstTypeId::UNIT);
}

#[test]
fn test_invert_requires_bool() {
    let ast = ast_from_text("fn inv(x: ())->bool{\n!x}");
    let err = ast.unwrap_err().1;

    assert_eq!(err.len(), 1);
    let (ctx, from, to) = unwrap_variant!(&err[0], AstError::TypeConversion, 3);
    assert_eq!(ctx.error_pos, Pos::new(2, 1));
    assert_eq!(
        ctx.kind,
        AstErrorContextKind::UnaryOp(TokenKind::Exclamation)
    );
    assert_eq!(from, &AstTypeId::UNIT);
    assert_eq!(to, &AstTypeId::BOOL);
}
