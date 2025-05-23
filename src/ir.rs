use crate::AST;
use crate::ast;
use crate::cst;
use crate::tokens::Pos;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IRReg(pub usize);

impl IRReg {
    pub const UNIT: IRReg = IRReg(0);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IRCodeBlockId(pub usize);

#[derive(Debug)]
pub enum IROp {
    /// Call a code block withtin a function,
    /// after the call is complete, PC is set to the next instruction after the call
    LocalCall {
        block_id: IRCodeBlockId,
        dest: IRReg,
    },
    /// Return a register from the function
    Return { value: IRReg },
}

#[derive(Debug)]
pub struct IRCodeBlock {
    pub id: IRCodeBlockId,
    pub ops: Vec<IROp>,
}

impl IRCodeBlock {
    fn new(id: IRCodeBlockId) -> Self {
        Self {
            id,
            ops: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct IRFunction {
    pub pos: Pos,
    pub name: String,
    pub blocks: Vec<IRCodeBlock>,
    /// Regs. For now each reg is nothing but its own index
    /// We'll keep it as vec for future
    pub regs: Vec<IRReg>,
}

impl IRFunction {
    pub fn new<S: Into<String>>(name: S, pos: Pos) -> Self {
        Self {
            name: name.into(),
            blocks: vec![],
            pos,
            regs: vec![IRReg::UNIT],
        }
    }

    fn new_reg(&mut self) -> IRReg {
        let reg_id = IRReg(self.regs.len());
        self.regs.push(reg_id);
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

    fn translate_expr(&mut self, ir_fun: &mut IRFunction, ast_expr: &ast::Expr) -> IRReg {
        match &ast_expr.kind {
            ast::ExprKind::Unit => IRReg::UNIT,
            _ => unimplemented!("expression parser nyi for {:?}", &ast_expr.kind),
        }
    }

    fn translate_code_block(
        &mut self,
        ir_fun: &mut IRFunction,
        ast_code_block: &ast::CodeBlock,
    ) -> IRCodeBlockId {
        let mut block = ir_fun.prepare_block();

        for x in &ast_code_block.exprs {
            // This is a AST code block, where everything is expression.
            // For IR we have OPs
            match &x.kind {
                ast::ExprKind::Return(value) => {
                    /*Return an expression(or () if no expression is provided)*/
                    let ret = match value.as_ref() {
                        Some(expr) => {
                            let expr_reg = self.translate_expr(ir_fun, expr);
                            IROp::Return { value: expr_reg }
                        }
                        None => IROp::Return { value: IRReg::UNIT },
                    };
                    block.ops.push(ret);
                }
                ast::ExprKind::CodeBlock(nested_block) => {
                    // Nessted block, a case of
                    // fn main() {
                    //    {
                    //    }
                    // }
                    // It may have on AST level which inserts drops for affine types, but
                    // on IR level we just translate it into series of branches

                    let blk = self.translate_code_block(ir_fun, nested_block);
                    let dest_reg = ir_fun.new_reg();
                    block.ops.push(IROp::LocalCall {
                        block_id: blk,
                        dest: dest_reg,
                    });
                }
                ast::ExprKind::Unit => {
                    // Unit() on top-expr(stmt) level is essentially nop.
                }
            }
        }

        ir_fun.insert_block(block)
    }

    fn translate_ast_func(&mut self, ast_func: &ast::Function) -> Result<IRFunction, String> {
        let mut fun = IRFunction::new(&ast_func.name, ast_func.decl_pos);

        let body = match ast_func.body.as_ref() {
            Some(body) => body,
            None => unimplemented!("IR doesn't support extern functions"),
        };

        self.translate_code_block(&mut fun, body);

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
            match ir.translate_ast_func(func) {
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
                return Err(err.1);
            }
        };
        let ast = match AST::from_cst(cst) {
            Ok(ast) => ast,
            Err(err) => {
                return Err(err.1);
            }
        };
        IR::from_ast(&ast)
    }
}

#[cfg(test)]
#[path = "_tests/test_ir.rs"]
mod test_ir;
