use crate::AST;
use crate::ast::{AstErrorContextKind, ExprKind};
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
fn test_invert_is_not_unit_type() {
    let ast = ast_from_text("fn inv(x: bool) {\n!x}");
    let err = ast.unwrap_err().1;

    assert_eq!(err.len(), 1);
    let (ctx, from, to) = unwrap_variant!(&err[0], AstError::TypeConversion, 3);
    assert_eq!(ctx.error_pos, Pos::new(2, 1));
    assert_eq!(ctx.kind, AstErrorContextKind::FunctionReturn);
    assert_eq!(from.0, "bool");
    assert_eq!(to.0, "()");
}

#[test]
fn test_invert_requires_bool() {
    let ast = ast_from_text("fn inv(x: ()){\n!x}");
    let err = ast.unwrap_err().1;

    assert_eq!(err.len(), 1);
    let (ctx, from, to) = unwrap_variant!(&err[0], AstError::TypeConversion, 3);
    assert_eq!(ctx.error_pos, Pos::new(2, 1));
    assert_eq!(
        ctx.kind,
        AstErrorContextKind::UnaryOp(TokenKind::Exclamation)
    );
    assert_eq!(from.0, "()");
    assert_eq!(to.0, "bool");
}
