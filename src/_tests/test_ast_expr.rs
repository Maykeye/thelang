use crate::AST;
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
}

#[test]
fn test_invert_is_not_unit_type() {
    let ast = ast_from_text("fn inv(x: bool) {!x}");
    unimplemented!("Test error talks about bad type for returning bool as it's not unit")
}

#[test]
fn test_invert_requires_bool() {
    let ast = ast_from_text("fn inv(x: ()){!x}");
    unimplemented!("Test error talks about bad type for !operation")
}
