use crate::AST;
use crate::ast;
use crate::cst;
use crate::tokens::Pos;
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IRReg(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IRTypeId(pub usize);

impl IRTypeId {
    pub const NEVER: IRTypeId = IRTypeId(0);
    pub const UNIT: IRTypeId = IRTypeId(1);
    pub const BOOL: IRTypeId = IRTypeId(2);
}

#[derive(Debug, PartialEq, Eq)]
pub struct IRType {
    pub name: String,
}

impl IRType {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

impl IRReg {
    pub const UNIT: IRReg = IRReg(0);
    pub const BUILTIN_REGS_COUNT: usize = 1;
}

#[derive(Debug)]
pub struct IRRegData {
    pub id: IRReg,
    pub name: Option<String>,
    pub r#type: IRTypeId,
}

impl IRRegData {
    pub fn new(id: IRReg, name: Option<String>, r#type: IRTypeId) -> Self {
        Self { id, name, r#type }
    }

    pub fn new_unit() -> IRRegData {
        Self::new(IRReg::UNIT, Some("()".to_string()), IRTypeId::UNIT)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IRCodeBlockId(pub usize);

impl Display for IRCodeBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".b{}", self.0)
    }
}

#[derive(Debug)]
pub enum IROp {
    /// Call a code block withtin a function,
    /// after the call is complete, PC is set to the next instruction after the call
    LocalCall {
        block_id: IRCodeBlockId,
        dest: IRReg,
    },
    /// Return a register from the function
    Return {
        value: IRReg,
    },

    /// Invert boolean value
    Invert {
        value: IRReg,
        dest: IRReg,
    },

    /// Load argument
    LoadArg {
        arg: IRReg,
        dest: IRReg,
    },

    LdConstBool {
        value: bool,
        dest: IRReg,
    },
}

#[derive(Debug)]
pub struct IRCodeBlock {
    pub id: IRCodeBlockId,
    pub ops: Vec<IROp>,
    pub type_id: IRTypeId,
}

impl IRCodeBlock {
    fn new(id: IRCodeBlockId) -> Self {
        Self {
            id,
            ops: Default::default(),
            type_id: IRTypeId::UNIT,
        }
    }
}

#[derive(Debug)]
pub struct IRFunction {
    pub pos: Pos,
    pub name: String,
    pub blocks: Vec<IRCodeBlock>,
    pub args: Vec<IRReg>,
    pub regs: Vec<IRRegData>,
    pub types: Vec<IRType>,
}

impl IRFunction {
    pub fn new<S: Into<String>>(name: S, pos: Pos) -> Self {
        let types = vec![IRType::new("!"), IRType::new("()"), IRType::new("bool")];
        Self {
            name: name.into(),
            blocks: vec![],
            pos,
            regs: vec![IRRegData::new_unit()],
            args: vec![],
            types,
        }
    }

    fn new_reg(&mut self, type_id: IRTypeId, name: Option<String>) -> IRReg {
        let reg_id = IRReg(self.regs.len());
        self.regs.push(IRRegData::new(reg_id, name, type_id));
        reg_id
    }

    /// Prepares block to insert without actually inserting it yet(dummy will be inserted instead)
    pub fn prepare_block(&mut self) -> IRCodeBlock {
        let id = IRCodeBlockId(self.blocks.len());
        let dummy = IRCodeBlock::new(id);
        let work = IRCodeBlock::new(id);

        // since type-checking was done on AST level, we don't care that we insert dummy block
        // with type unknown until it's replaced.
        // Eg consider if cond {then} else {else}: here both then and else must match,
        // but we don't care about their types on IR level.
        self.blocks.push(dummy);
        work
    }

    fn insert_block(&mut self, block: IRCodeBlock) -> IRCodeBlockId {
        // Blocks should be prepared via `prepare_block`.
        // `prepare_block` invariant is its.id is always in bounds of `blocks` list
        if block.id.0 >= self.blocks.len() {
            panic!(
                "Internal error: block {:?} is out of bounds: {:?}",
                block.id, block
            );
        }
        let id = block.id;
        self.blocks[id.0] = block;
        id
    }

    pub fn get_reg_data(&self, reg: IRReg) -> &IRRegData {
        &self.regs[reg.0]
    }
    pub fn get_block_type(&self, block: IRCodeBlockId) -> IRTypeId {
        self.blocks[block.0].type_id
    }

    pub fn format_reg_name(&self, reg: IRReg) -> String {
        let data = self.get_reg_data(reg);
        // TODO: different prefix for different registers
        match &data.name {
            Some(name) => format!("$r{}:<{name}>", reg.0),
            None => format!("$r{}", reg.0),
        }
    }
}

#[derive(Debug)]
pub struct IR {
    pub functions: HashMap<String, IRFunction>,
}

impl IR {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
        }
    }
    pub fn to_text(&self) -> String {
        let mut s = String::new();

        for func in self.functions.values() {
            s.push_str("FUNC ");
            s.push_str(&func.name);
            s.push('\n');
            // TODO: print args

            for blk in func.blocks.iter() {
                s.push_str(&format!("{}:\n", blk.id));
                for op in blk.ops.iter() {
                    let ins = match op {
                        IROp::LocalCall { block_id, dest } => {
                            format!("{} = call {}", func.format_reg_name(*dest), block_id)
                        }
                        IROp::Return { value } => format!("ret {}", func.format_reg_name(*value)),
                        IROp::LoadArg { arg, dest } => format!(
                            "{} = ld.arg {}",
                            func.format_reg_name(*dest),
                            func.format_reg_name(*arg),
                        ),
                        IROp::LdConstBool { value, dest } => {
                            format!("{} = ld.const.b {}", func.format_reg_name(*dest), value)
                        }
                        IROp::Invert { value, dest } => format!(
                            "{} = invert.bool {}",
                            func.format_reg_name(*dest),
                            func.format_reg_name(*value)
                        ),
                    };
                    s.push_str(&ins);
                    s.push('\n');
                }
                s.push('\n');
            }
            s.push_str("END FUNC ");
            s.push_str(&func.name);
            s.push('\n');
        }
        s
    }

    fn map_ast_arg_index_to_ir_arg_index(
        &mut self,
        ast_arg_index: usize,
        _ir_fun: &IRFunction,
        _ast_func: &ast::Function,
    ) -> usize {
        ast_arg_index
    }

    fn parse_expr(
        &mut self,
        ast_expr: &ast::Expr,
        ir_fun: &mut IRFunction,
        ir_block: &mut IRCodeBlock,
        ast_func: &ast::Function,
    ) -> IRReg {
        match &ast_expr.kind {
            ast::ExprKind::Unit => IRReg::UNIT,
            ast::ExprKind::Argument(name) => {
                let ast_arg_idx = ast_func
                    .get_argument_index_by_name(name)
                    .unwrap_or_else(|| {
                        panic!("Internal error: AST was passed with inconsistent arguments: {name}")
                    });
                let ir_arg_idx =
                    self.map_ast_arg_index_to_ir_arg_index(ast_arg_idx, ir_fun, ast_func);

                let arg_reg = *ir_fun.args.get(ir_arg_idx).unwrap_or_else(|| {
                    panic!(
                        "Internal error: IR  has inconsistent argument @{name}: {:?}",
                        ir_fun.args
                    )
                });
                let data = ir_fun.get_reg_data(arg_reg);
                let op_reg = ir_fun.new_reg(data.r#type, None);

                ir_block.ops.push(IROp::LoadArg {
                    arg: arg_reg,
                    dest: op_reg,
                });

                op_reg
            }
            ast::ExprKind::Invert(base) => {
                let base_reg = self.parse_expr(base, ir_fun, ir_block, ast_func);
                let op_reg = ir_fun.new_reg(IRTypeId::BOOL, None);
                ir_block.ops.push(IROp::Invert {
                    value: base_reg,
                    dest: op_reg,
                });
                op_reg
            }
            _ => unimplemented!("expression parser nyi for {:?}", &ast_expr.kind),
        }
    }

    fn parse_code_block(
        &mut self,
        ir_fun: &mut IRFunction,
        ast_fn: &ast::Function,
        ast_code_block: &ast::CodeBlock,
    ) -> IRCodeBlockId {
        let mut block = ir_fun.prepare_block();

        let mut last_reg = None;
        let mut has_branch = false;

        for x in &ast_code_block.exprs {
            // This is a AST code block, where everything is expression.
            // For IR we have OPs
            match &x.kind {
                ast::ExprKind::BooleanLiteral(value) => {
                    let dest = ir_fun.new_reg(IRTypeId::BOOL, None);
                    let op = IROp::LdConstBool {
                        value: *value,
                        dest,
                    };

                    block.ops.push(op);
                    last_reg = Some(dest);
                }
                ast::ExprKind::Return(value) => {
                    /*Return an expression(or () if no expression is provided)*/
                    let ret = match value.as_ref() {
                        Some(expr) => {
                            let expr_reg = self.parse_expr(expr, ir_fun, &mut block, ast_fn);
                            IROp::Return { value: expr_reg }
                        }
                        None => IROp::Return { value: IRReg::UNIT },
                    };
                    block.ops.push(ret);
                    block.type_id = IRTypeId::NEVER;
                    has_branch = true;
                }
                ast::ExprKind::CodeBlock(nested_block) => {
                    // Nested block, a case of
                    // fn main() {
                    //    {
                    //    }
                    // }
                    // It may have on AST level which inserts drops for affine types, but
                    // on IR level we just translate it into series of branches

                    let blk = self.parse_code_block(ir_fun, ast_fn, nested_block);
                    let dest_reg = ir_fun.new_reg(ir_fun.get_block_type(blk), None);
                    block.ops.push(IROp::LocalCall {
                        block_id: blk,
                        dest: dest_reg,
                    });
                    last_reg = Some(dest_reg);
                }
                ast::ExprKind::Unit => {
                    last_reg = Some(IRReg::UNIT)
                    // Unit() on top-expr(stmt) level is essentially nop.
                }
                ast::ExprKind::Invert(_) | ast::ExprKind::Argument(_) => {
                    let reg = self.parse_expr(x, ir_fun, &mut block, ast_fn);
                    last_reg = Some(reg);
                }
            }
        }

        if !has_branch {
            let return_reg = last_reg.unwrap_or(IRReg::UNIT);
            let op = IROp::Return { value: return_reg };
            block.ops.push(op);
            block.type_id = ir_fun.get_reg_data(return_reg).r#type;
        }

        ir_fun.insert_block(block)
    }

    /// Parse AST type.
    /// Identical types will be merged
    fn parse_type(&mut self, ast_type: &ast::Type) -> IRTypeId {
        match ast_type {
            ast::Type::Unit => IRTypeId::UNIT,
            ast::Type::Never => IRTypeId::NEVER,
            _ => unimplemented!("unimplemented type parsing for {ast_type:?}"),
        }
    }

    fn parse_ast_func(&mut self, ast_func: &ast::Function) -> Result<IRFunction, String> {
        let mut fun = IRFunction::new(&ast_func.name, ast_func.decl_pos);

        for arg in ast_func.r#type.args.iter() {
            let type_id = self.parse_type(&arg.r#type);
            // TODO: give register an argument flag, not just put in args
            let arg_reg = fun.new_reg(type_id, Some(arg.name.clone()));
            fun.args.push(arg_reg);
        }

        match ast_func.body.as_ref() {
            Some(body) => {
                self.parse_code_block(&mut fun, ast_func, body);
            }
            None => unimplemented!("IR doesn't support extern functions"),
        }

        let check = self.functions.get(&ast_func.name);

        if let Some(other) = check {
            let msg = ast_func.decl_pos.report(format!(
                "error: IR has the function with the same name `{}` at {:?}",
                &ast_func.name, other.pos
            ));
            Err(msg)
        } else {
            Ok(fun)
        }
    }

    pub fn from_ast(ast: &AST) -> Result<Self, Vec<String>> {
        let mut ir = IR::new();
        let mut errs = vec![];

        for func in ast.functions.values() {
            match ir.parse_ast_func(func) {
                Ok(ir_fun) => {
                    ir.functions.insert(ir_fun.name.clone(), ir_fun);
                }
                Err(err) => {
                    errs.push(err);
                }
            }
        }
        Ok(ir)
    }

    pub fn from_thelan<S: Into<String>>(source: S) -> Result<IR, Vec<String>> {
        // TODO:  use error instead of stings
        let source: String = source.into();
        let tokens = match crate::lexer::tokenize(&source) {
            Ok(tokens) => tokens,
            Err(err) => {
                return Err(vec![err]);
            }
        };

        let cst = match cst::CST::from_tokens(&tokens) {
            Ok(cst) => cst,
            Err(err) => {
                return Err(err.1.iter().map(|err| format!("{err:?}")).collect());
            }
        };
        let ast = match AST::from_cst(cst) {
            Ok(ast) => ast,
            Err(err) => {
                return Err(err.1.iter().map(|err| format!("{err:?}")).collect());
            }
        };
        IR::from_ast(&ast)
    }
}

#[cfg(test)]
#[path = "_tests/test_ir.rs"]
mod test_ir;
