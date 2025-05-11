use std::collections::HashMap;

use tokens::{CharPos, Pos, Token, TokenKind, Tokens};
mod tokens;

use cst::CST;
use lexer::tokenize;
mod cst;
mod lexer;

fn main() {
    let source = " fn main() {} ";
    let tokens = tokenize(source).unwrap();
    let cst = CST::from_tokens(&tokens).unwrap();
    println!("{:?}", cst)
}
