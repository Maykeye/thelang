use std::collections::HashMap;

use crate::{
    cst::{self, CST},
    tokens::Pos,
};

#[derive(Debug, PartialEq, Eq, Clone)]
struct TpFunctionArg {
    name: Option<String>,
    r#type: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TpFunction {
    args: Vec<TpFunctionArg>,
    return_type: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Function(Box<TpFunction>),
    /// Unit type. Has exactly one instance ()
    Unit,
    /// `!` is a so called never type, which has no instances
    Never,
}

#[derive(Debug)]
pub enum ExprKind {
    Unit,
    Return(Option<Box<Expr>>),
    CodeBlock(CodeBlock),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub pos: Pos,
    pub r#type: Option<Type>,
}
impl Expr {
    pub fn new(kind: ExprKind, pos: Pos, r#type: Option<Type>) -> Self {
        Self { kind, pos, r#type }
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    /// Expressions; we don't have statements
    pub exprs: Vec<Expr>,
    /// Position of the start of the block
    pub pos: Pos,
    /// Return type of the block: if block is interrupted by return <expr>, <expr> type is stored
    /// there. If no interruption found, type is none.
    /// This is to allow differentiate between `10 + {3}` and `10 + {return 3}`
    pub return_type: Option<Type>,
}

impl CodeBlock {
    fn new(pos: Pos) -> Self {
        return Self {
            exprs: Default::default(),
            pos,
            return_type: None,
        };
    }

    fn last_expr_pos(&self) -> Pos {
        self.exprs.last().map_or(self.pos, |e| e.pos)
    }
}

#[derive(Debug)]
pub struct Function {
    pub decl_pos: Pos,
    pub name: String,
    pub r#type: TpFunction,
    pub body: Option<CodeBlock>,
}

#[derive(Debug)]
pub struct AST {
    pub functions: HashMap<String, Function>,
}

impl AST {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
        }
    }

    fn parse_cst_function_declarations<'a>(
        &mut self,
        cst: &'a CST,
        errors: &mut Vec<String>,
    ) -> HashMap<String, &'a cst::Fn> {
        let mut mappings = HashMap::default();
        // Prepare functions headers
        for cst_func in cst.functions.values() {
            let args = vec![];

            // Ensure such function doesn't override existing function
            let full_name = cst_func.name.clone();
            if let Some(other) = self.functions.get(&full_name) {
                let msg = cst_func.pos.report(format!(
                    "function `{}` already declared at {:?}",
                    &full_name, other.decl_pos
                ));
                errors.push(msg);
                continue;
            }

            // Parse arguments
            if !cst_func.args.is_empty() {
                unimplemented!("TBD: arguments NYI")
            }

            // Parse return type
            let func_ret_type = match cst_func.return_type {
                Some(_) => unimplemented!("TBD: return type NYI"),
                None => Type::Unit,
            };

            // Assign function type to the global symbol
            let tp_func = TpFunction {
                args,
                return_type: func_ret_type,
            };
            let func = Function {
                decl_pos: cst_func.pos,
                name: full_name.clone(),
                r#type: tp_func,
                body: None,
            };

            mappings.insert(full_name.clone(), cst_func);
            self.functions.insert(full_name, func);
        }
        mappings
    }

    fn check_type_implicit_conversion(
        pos: Pos,
        from: &Type,
        to: &Type,
        errors: &mut Vec<String>,
    ) -> bool {
        if *from == *to {
            return true;
        }
        errors.push(pos.report(format!("can't convert type {:?} to {:?}", from, to)));
        return false;
    }

    fn parse_expr(cst_expr: &cst::Node) -> Expr {
        match &cst_expr.kind {
            cst::NodeKind::Unit => Expr::new(ExprKind::Unit, cst_expr.pos, Some(Type::Unit)),
            _ => unimplemented!("nyi"),
        }
    }

    fn parse_code_block(cst: &cst::CodeBlock, errors: &mut Vec<String>) -> Option<CodeBlock> {
        let pos = cst.pos;
        let mut code_block = CodeBlock::new(pos);
        // Empty body = return ()
        if cst.nodes.is_empty() {
            let r#return = Expr::new(ExprKind::Return(None), pos, Some(Type::Never));
            code_block.exprs.push(r#return);
            code_block.return_type = Some(Type::Unit);
            return Some(code_block);
        }

        // Convert CST untyped nodes to AST typed nodes
        for node in cst.nodes.iter() {
            match &node.kind {
                cst::NodeKind::Return(val) => {
                    // Parse conversion
                    let expr = Self::parse_expr(val);

                    // If it's the second return type, make sure they are convertible
                    assert!(expr.r#type.is_some(), "{}", node.pos.report("unknown type"));
                    if let Some(current_return_type) = code_block.return_type {
                        let new_type = expr.r#type.as_ref().unwrap();
                        // TODO: return or not?
                        Self::check_type_implicit_conversion(
                            node.pos,
                            new_type,
                            &current_return_type,
                            errors,
                        );
                    }

                    // Return
                    code_block.return_type = expr.r#type.clone();
                    let expr = Expr::new(
                        ExprKind::Return(Some(Box::new(expr))),
                        pos,
                        Some(Type::Never),
                    );
                    code_block.exprs.push(expr);
                }

                cst::NodeKind::Unit => {
                    code_block
                        .exprs
                        .push(Expr::new(ExprKind::Unit, node.pos, Some(Type::Unit)));
                }

                _ => unimplemented!("tbd: parse_code_block: {:?}", &node.kind),
            }
        }

        return Some(code_block);
    }

    fn parse_cst_function_definition<'a>(
        &mut self,
        errors: &mut Vec<String>,
        mappings: &HashMap<String, &'a cst::Fn>,
    ) {
        for func in self.functions.values_mut() {
            let cst_func = *mappings.get(&func.name).expect(&format!(
                "Internal error: can't find the CST body of {}",
                &func.name
            ));

            let cst_body = match &cst_func.body {
                Some(body) => body,
                None => {
                    let msg = func
                        .decl_pos
                        .report(format!("{}: function body doesn't exist", &func.name));
                    errors.push(msg);
                    continue;
                }
            };

            let mut code_block = match Self::parse_code_block(cst_body, errors) {
                Some(blk) => blk,
                None => {
                    continue;
                }
            };

            // Resolve return type:
            // If function body has no explicit return, then take the last-known-expression type.
            // At this point expression types must be all resolved
            // Default type is unit, of course
            if code_block.return_type == None {
                let tp = match code_block.exprs.last() {
                    Some(e) => {
                        let t = match e.r#type.as_ref() {
                            Some(t) => t.clone(),
                            None => {
                                let msg =  code_block.pos.report( "internal error: code block last expression type is not resolved");
                                panic!("{}", msg)
                            }
                        };
                        t
                    }
                    None => Type::Unit,
                };
                code_block.return_type = Some(tp);
            }

            // At this point all expresions must be parsed
            // We need to make sure return type can be converted to the function return type
            match code_block.return_type.as_ref() {
                None => {
                    // TODO: last expr type
                    let msg = code_block
                        .last_expr_pos()
                        .report(format!("internal error: no type for function body found"));
                    errors.push(msg);
                    continue;
                }

                Some(r#type) => {
                    Self::check_type_implicit_conversion(
                        code_block.last_expr_pos(),
                        r#type,
                        &func.r#type.return_type,
                        errors,
                    );
                }
            }

            func.body = Some(code_block);
        }
    }

    pub fn from_cst(cst: CST) -> Result<AST, (AST, Vec<String>)> {
        let mut ast = AST::new();
        let mut errors = vec![];
        let fn_mappings = ast.parse_cst_function_declarations(&cst, &mut errors);
        ast.parse_cst_function_definition(&mut errors, &fn_mappings);

        if errors.is_empty() {
            Ok(ast)
        } else {
            Err((ast, errors))
        }
    }
}

#[cfg(test)]
#[path = "_tests/test_ast.rs"]
mod test_ast;
