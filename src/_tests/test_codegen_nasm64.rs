use crate::{codegen::CodeGen, ir::IR};

use super::CodeGenNasm64;
use pretty_assertions::assert_eq;

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
    push rbp
    mov rbp, rsp
    sub rsp, qword 0
.b0:
    leave
    ret
    leave
    ret
    leave
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
    push rbp
    mov rbp, rsp
    sub rsp, qword 0
.b0:
    call .b1
    leave
    ret

.b1:
    call .b2
    ret

.b2:
    ret
";
    assert_eq!(text, expected);
}

#[test]
fn test_load_bool() {
    let text = compile_to_text("fn main(a:bool) -> bool {a}");
    let expected = "\
global main

section .text

main:
    push rbp
    mov rbp, rsp
    sub rsp, qword 1
.b0:
    mov byte [rbp-1], r15b
    mov al, [rbp-1]
    leave
    ret
";
    assert_eq!(text, expected);
}
