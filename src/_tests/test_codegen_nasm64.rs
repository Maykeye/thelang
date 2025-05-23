use crate::{codegen::CodeGen, ir::IR};

use super::CodeGenNasm64;

fn compile_to_text(source: &str) -> String {
    let ir = IR::from_thelan(source).unwrap();
    let nasm = CodeGenNasm64::from_ir(&ir).unwrap();
    nasm.to_text().unwrap().join("\n")
}

#[test]
fn test_triple_ret() {
    let text = compile_to_text(" fn main() {\nreturn;return();\nreturn } ");
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

#[test]
fn test_nested_block() {
    let text = compile_to_text("fn main() {{{}}}");
    let expected = "\
global main

section .text

main:
.b0:
    call .b1
    ret

.b1:
    call .b2
    ret

.b2:
    ret
";
    assert_eq!(text, expected);
}
