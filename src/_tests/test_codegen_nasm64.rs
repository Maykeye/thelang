use crate::{codegen::CodeGen, ir::IR};

use super::CodeGenNasm64;

#[test]
fn test_triple_ret() {
    let ir = IR::from_thelan(" fn main() {\nreturn;return();\nreturn } ").unwrap();
    let nasm = CodeGenNasm64::from_ir(&ir).unwrap();
    let text = nasm.to_text().unwrap().join("\n");
    let expected = "\
global main

section .text

main:
.b0:
    ret
    ret
    ret
";
    assert_eq!(text, expected);
}
