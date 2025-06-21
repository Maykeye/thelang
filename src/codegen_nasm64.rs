use std::{fmt::Display, vec};

use crate::{
    IR,
    codegen::CodeGen,
    ir::{IRCodeBlockId, IRFunction, IROp, IRTypeId},
};

/// Target OS for nasm64, for now only one is supported
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CodeGenNasm64TargetOS {
    Linux,
}

/// Config for nasm64 based code generation
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct CodeGenNasm64Config {
    pub should_implement_start: bool,
    pub target_os: CodeGenNasm64TargetOS,
}

impl Default for CodeGenNasm64Config {
    fn default() -> Self {
        Self {
            should_implement_start: false,
            target_os: CodeGenNasm64TargetOS::Linux,
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
enum Operand {
    RAX,
    RDI,
    U64(u64),
    Label(String),
    CodeBlock(IRCodeBlockId),
}

fn rax() -> Operand {
    Operand::RAX
}
fn rdi() -> Operand {
    Operand::RDI
}
fn label<S: Into<String>>(s: S) -> Operand {
    Operand::Label(s.into())
}
fn u64(value: u64) -> Operand {
    Operand::U64(value)
}
fn cb_id(id: IRCodeBlockId) -> Operand {
    Operand::CodeBlock(id)
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::RAX => "rax".fmt(f),
            Operand::RDI => "rdi".fmt(f),
            Operand::U64(v) => write!(f, "qword {v}"),
            Operand::Label(l) => l.fmt(f),
            Operand::CodeBlock(block_id) => write!(f, ".b{}", block_id.0),
        }
    }
}
struct Builder {
    buffer: Vec<String>,
}

impl Builder {
    fn new() -> Self {
        Self { buffer: vec![] }
    }

    // General functions
    /// Add a new string to our buffer
    fn str<S: Into<String>>(&mut self, code_line: S) {
        self.buffer.push(code_line.into());
    }
    /// Write a line using a single level of indentation
    fn indented<D: Display>(&mut self, d: D) {
        self.str(format!("    {d}"));
    }

    /// Write empty line
    fn nl(&mut self) {
        self.str("");
    }

    // General meta non-instructions
    fn section(&mut self, section: &str) {
        self.str(format!("section {section}"));
    }
    fn global(&mut self, label: &str) {
        self.str(format!("global {label}"));
    }
    fn comment(&mut self, text: &str) {
        self.indented(format!("; {text}"));
    }
    fn label<D: Display>(&mut self, label: D) {
        self.str(format!("{label}:"));
    }
    fn cb_label(&mut self, id: IRCodeBlockId) {
        self.label(format!("{}", cb_id(id)))
    }
    // General instructions
    fn mov(&mut self, dest: Operand, src: Operand) {
        self.indented(format!("mov {}, {}", dest, src));
    }
    fn ret(&mut self) {
        self.indented("ret");
    }
    fn call(&mut self, func: Operand) {
        self.indented(format!("call {func}"));
    }
    fn syscall(&mut self) {
        self.indented("syscall");
    }
}

pub struct CodeGenNasm64 {
    code: Builder,
    config: CodeGenNasm64Config,
}

impl CodeGenNasm64 {
    pub fn new() -> Self {
        Self {
            code: Builder::new(),
            config: CodeGenNasm64Config::default(),
        }
    }

    /// Check if IR has main() function
    /// Main requires special treatment around it.
    fn ir_has_main(ir: &IR) -> bool {
        ir.functions.contains_key("main")
    }

    /// If we have functions to export, write them.
    fn write_header_global_functions(&mut self, ir: &IR) {
        if self.config.should_implement_start && Self::ir_has_main(ir) {
            self.code.global("_start");
        }

        // TODO: add pub/priv keywords for function exprts?
        for ir_func in ir.functions.values() {
            self.code.global(&ir_func.name)
        }
    }

    /// Writes _start function
    /// _start function calls main(),
    /// and if returns, we call exit(0)
    fn write_start_linux(&mut self) {
        self.code.label("_start");
        self.code.call(label("main"));
        self.code.nl();

        // TODO: choose OS
        self.code.comment("Call exit(0) syscall");
        self.code.mov(rax(), u64(60));
        self.code.mov(rdi(), u64(0));
        self.code.syscall();
        self.code.nl();
    }

    fn write_start(&mut self) {
        match self.config.target_os {
            CodeGenNasm64TargetOS::Linux => self.write_start_linux(),
        }
    }

    /// Write a header of the file:
    /// exportted globals,
    /// and a _start function.
    fn write_header(&mut self, ir: &IR) {
        // Write global functions
        self.write_header_global_functions(ir);
        self.code.nl();
        // Start the code section
        self.code.section(".text");
        self.code.nl();

        // _start is written only if there is a main() function.
        if self.config.should_implement_start && Self::ir_has_main(ir) {
            self.write_start();
        }
    }

    fn write_code_block(&mut self, _ir: &IR, ir_func: &IRFunction, code_block_id: IRCodeBlockId) {
        let code_block = ir_func.blocks.get(code_block_id.0).unwrap_or_else(|| {
            panic!(
                "internal error: invalid code block id {}:{}",
                ir_func.name, code_block_id.0
            )
        });
        self.code.cb_label(code_block_id);
        for op in &code_block.ops {
            match op {
                IROp::Return { value: irreg } => {
                    // TODO: once we'll implement virtual registers and mappings, we'll do
                    // mov rax, [rel irreg.offset]
                    let tp = ir_func.get_reg_data(*irreg).r#type;
                    assert!(
                        (tp == IRTypeId::UNIT) || (tp == IRTypeId::NEVER),
                        "currently only () register is supported"
                    );
                    self.code.ret();
                }
                IROp::LocalCall {
                    block_id: other_code_block_id,
                    dest: _dest,
                } => {
                    self.code.call(Operand::CodeBlock(*other_code_block_id));
                }

                IROp::LdConstBool { value, dest } => {
                    unimplemented!();
                }
                IROp::LoadArg {
                    arg: arg_reg,
                    dest: value_reg,
                } => {
                    let reg_data = ir_func.get_reg_data(*value_reg);
                    // Unit type has no need for actual register or variable on stack
                    if reg_data.r#type == IRTypeId::UNIT {
                        self.code
                            .comment(&format!("{} = ()", ir_func.format_reg_name(*value_reg)));
                    } else {
                        unimplemented!();
                    }
                }

                IROp::Invert { value, dest } => {
                    unimplemented!();
                }
            }
        }
    }

    fn write_function_def(&mut self, ir: &IR, ir_func: &IRFunction) {
        self.code.label(&ir_func.name);
        for i in 0..ir_func.blocks.len() {
            self.write_code_block(ir, ir_func, IRCodeBlockId(i));
            self.code.nl();
        }
    }
}

impl CodeGen for CodeGenNasm64 {
    type Error = String;

    fn from_ir(ir: &IR) -> Result<Self, Self::Error> {
        let mut cg = Self::new();
        cg.write_header(ir);
        // TODO: write extern functions declarations
        // Write function definitions
        for ir_func in ir.functions.values() {
            if !ir_func.blocks.is_empty() {
                cg.write_function_def(ir, ir_func);
            }
        }

        Ok(cg)
    }

    fn to_text(&self) -> Result<Vec<String>, Self::Error> {
        Ok(self.code.buffer.clone())
    }

    fn decode_error(error: &Self::Error) -> String {
        error.clone()
    }
}

#[cfg(test)]
#[path = "_tests/test_codegen_nasm64.rs"]
mod test_codegen_nasm64;
