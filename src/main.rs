use tokens::{CharPos, Pos, Token, TokenKind};
mod ir;
mod tokens;
use ast::AST;
use cst::CST;
use ir::IR;
use lexer::tokenize;
mod ast;
mod cst;
mod lexer;

fn main() {
    let source = " fn main() {} ";
    let tokens = tokenize(source).unwrap();
    let cst = CST::from_tokens(&tokens).unwrap();
    let ast = AST::from_cst(cst).unwrap();
    let ir = IR::from_ast(&ast);
    println!("{:?}", ir)
}
