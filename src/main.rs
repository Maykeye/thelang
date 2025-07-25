use codegen_nasm64::CodeGenNasm64;
use tokens::{CharPos, Token, TokenKind};
mod ir;
mod tokens;
use ast::AST;
use cst::CST;
use ir::IR;
use lexer::tokenize;
mod ast;
mod codegen;
mod codegen_nasm64;
mod cst;
mod lexer;
mod test_tools;
use crate::codegen::CodeGen;

fn main() {
    let source = "fn and(a:bool, b:bool) -> bool { a & b } ";
    let tokens = tokenize(source).unwrap();
    let cst = CST::from_tokens(&tokens).unwrap();
    let ast = AST::from_cst(cst).unwrap();
    let ir = IR::from_ast(&ast).unwrap();
    let ir_text = ir.to_text();
    let cg_nasm64 = CodeGenNasm64::from_ir(&ir).unwrap();
    let lines = cg_nasm64.to_text().unwrap();
    println!("{}", ir_text);
    lines.iter().for_each(|s| println!("{}", s));
}
