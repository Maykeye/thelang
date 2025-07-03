use std::{collections::HashMap, fmt::Display, vec};

use crate::{
    IR,
    codegen::CodeGen,
    ir::{IRCodeBlockId, IRFunction, IROp, IRRegData, IRRegId, IRType, IRTypeId},
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum OperandSize {
    None,
    Byte,
}

impl Display for OperandSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            OperandSize::None => write!(f, ""),
            OperandSize::Byte => write!(f, "byte "),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Offset(isize);
impl Offset {
    fn new(sz: isize) -> Self {
        Self(sz)
    }
    fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:+}", self.0)
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, PartialEq, Eq)]
enum Operand {
    RAX,
    RDI,
    RSP,
    RBP,
    R15,
    U64(u64),
    Label(String),
    CodeBlock(IRCodeBlockId),
    Sized(OperandSize, Box<Operand>),
    Offset(Box<Operand>, Offset),
}

fn rax() -> Operand {
    Operand::RAX
}
fn rdi() -> Operand {
    Operand::RDI
}
fn rsp() -> Operand {
    Operand::RSP
}
fn rbp() -> Operand {
    Operand::RBP
}
fn r15() -> Operand {
    Operand::R15
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
        use Operand::*;
        use OperandSize::*;
        match self {
            RAX => "rax".fmt(f),
            RDI => "rdi".fmt(f),
            RSP => "rsp".fmt(f),
            RBP => "rbp".fmt(f),
            R15 => "r15".fmt(f),
            Offset(base_addr, delta) => write!(f, "[{base_addr}{delta}]"),
            U64(v) => write!(f, "qword {v}"),
            Label(l) => l.fmt(f),
            CodeBlock(block_id) => write!(f, ".b{}", block_id.0),
            Sized(Byte, reg) if reg.as_ref() == &RAX => "al".fmt(f),
            Sized(Byte, reg) if reg.as_ref() == &R15 => "r15l".fmt(f),
            Sized(Byte, offset) if matches!(offset.as_ref(), Operand::Offset(_, _)) => {
                write!(f, "byte {}", offset)
            }
            Sized(tp, op) => panic!("Invalid sized operand {op}:{tp}"),
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
    fn push(&mut self, operand: Operand) {
        self.indented(format!("push {operand}"));
    }
    fn sub(&mut self, dst: Operand, rhs: Operand) {
        self.indented(format!("sub {dst}, {rhs}"));
    }
    fn leave(&mut self) {
        self.indented("leave");
    }
}

pub struct CodeGenNasm64 {
    code: Builder,
    config: CodeGenNasm64Config,
}

type RegOffsets = HashMap<IRRegId, isize>;

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

    fn get_argument_operand(&self, reg_data: &IRRegData) -> Option<Operand> {
        let idx = reg_data.argument_index?;
        let is_primitive_arg = reg_data.r#type == IRTypeId::BOOL;
        if is_primitive_arg {
            match idx {
                0 => Some(Operand::R15),
                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }

    fn write_code_block(
        &mut self,
        _ir: &IR,
        ir_func: &IRFunction,
        code_block_id: IRCodeBlockId,
        locals: &RegOffsets,
    ) {
        let code_block = ir_func.blocks.get(code_block_id.0).unwrap_or_else(|| {
            panic!(
                "internal error: invalid code block id {}:{}",
                ir_func.name, code_block_id.0
            )
        });
        self.code.cb_label(code_block_id);
        for op in &code_block.ops {
            match op {
                IROp::Return {
                    value: return_value_reg,
                } => {
                    // TODO: local ret
                    // TODO: once we'll implement virtual registers and mappings, we'll do
                    // mov rax, [rel irreg.offset] for globals

                    let dat = ir_func.get_reg_data(*return_value_reg);
                    if dat.r#type == IRTypeId::BOOL {
                        assert!(dat.argument_index.is_none(), "returning arguments NYI");
                        let retval_ofset = locals
                            .get(&return_value_reg)
                            .expect("Dest register not found in locals");
                        let dst = Operand::Sized(OperandSize::Byte, Box::new(Operand::RAX));
                        self.code
                            .mov(dst, Operand::Offset(Box::new(rbp()), Offset(*retval_ofset)));
                    } else {
                        assert!(
                            (dat.r#type == IRTypeId::UNIT) || (dat.r#type == IRTypeId::NEVER),
                            "currently only bools, () and ! registers are supported"
                        );
                    }
                    self.code.leave();
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
                    let dst_reg_data = ir_func.get_reg_data(*value_reg);
                    // Unit type has no need for actual register or variable on stack
                    if dst_reg_data.r#type == IRTypeId::UNIT {
                        self.code
                            .comment(&format!("{} = ()", ir_func.format_reg_name(*value_reg)));
                    } else {
                        let src_reg_data = ir_func.get_reg_data(*arg_reg);
                        let src_arg_reg = self
                            .get_argument_operand(src_reg_data)
                            .expect("Argument operand expected");
                        let dst_offset = locals
                            .get(&dst_reg_data.id)
                            .expect("Dest register not found in locals");

                        let base_dst = Operand::Offset(Box::new(rbp()), Offset::new(*dst_offset));
                        let dst = if dst_reg_data.r#type == IRTypeId::BOOL {
                            Operand::Sized(OperandSize::Byte, Box::new(base_dst))
                        } else {
                            base_dst
                        };

                        self.code.mov(dst, src_arg_reg);
                    }
                }

                IROp::Invert { value, dest } => {
                    unimplemented!();
                }
            }
        }
    }

    fn reg_type_size(&self, ir_func: &IRFunction, type_id: IRTypeId) -> usize {
        match type_id {
            IRTypeId::BOOL => 1,
            IRTypeId::NEVER => 0,
            IRTypeId::UNIT => 0,
            _ => unimplemented!(),
        }
    }

    fn write_function_def(&mut self, ir: &IR, ir_func: &IRFunction) {
        self.code.label(&ir_func.name);

        // We'll ignore red zones and do the old way
        let mut local_reg_offsets = RegOffsets::new();
        let mut current_offset: isize = 0;
        for ir_reg in ir_func.regs.iter() {
            // Skip arguments
            if ir_reg.argument_index.is_some() {
                continue;
            }

            // Skip units
            let size = self.reg_type_size(ir_func, ir_reg.r#type) as isize;
            if size == 0 {
                continue;
            }
            current_offset -= size;
            local_reg_offsets.insert(ir_reg.id, current_offset);
        }
        self.code.push(rbp());
        self.code.mov(rbp(), rsp());
        // TODO: align with 16 bytes sysv requirement
        self.code.sub(rsp(), Operand::U64((-current_offset) as u64));

        for i in 0..ir_func.blocks.len() {
            self.write_code_block(ir, ir_func, IRCodeBlockId(i), &local_reg_offsets);
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
