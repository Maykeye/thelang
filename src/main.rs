use tokens::{CharPos, Token, TokenKind};
mod tokens;

use ast::AST;
use cst::CST;
use lexer::tokenize;
mod ast;
mod cst;
mod lexer;

fn main() {
    let source = " fn main() {} ";
    let tokens = tokenize(source).unwrap();
    let cst = CST::from_tokens(&tokens).unwrap();
    let ast = AST::from_cst(cst);
    println!("{:?}", ast)
}
