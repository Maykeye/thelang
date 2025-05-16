use std::collections::HashMap;

use crate::{
    cst::{self, CST},
    tokens::Pos,
};

#[derive(Debug, PartialEq, Eq)]
struct TpFunctionArg {
    name: Option<String>,
    r#type: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TpFunction {
    args: Vec<TpFunctionArg>,
    return_type: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Function(Box<TpFunction>),
    Unit,
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

#[derive(Debug)]
pub struct CodeBlock {
    pub exprs: Vec<Expr>,
    pub pos: Pos,
}

impl CodeBlock {
    fn new(pos: Pos) -> Self {
        return Self {
            exprs: Default::default(),
            pos,
        };
    }

    fn r#type(&self) -> Option<&Type> {
        self.exprs.last().and_then(|e| e.r#type.as_ref())
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
            let return_type = match cst_func.return_type {
                Some(_) => unimplemented!("TBD: return type NYI"),
                None => Type::Unit,
            };

            // Assign function type to the global symbol
            let tp_func = TpFunction { args, return_type };
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

    fn parse_code_block(
        pos: Pos,
        cst: &[cst::Node],
        errors: &mut Vec<String>,
    ) -> Option<CodeBlock> {
        let mut code_block = CodeBlock::new(pos);
        // Empty body = return ()
        if cst.is_empty() {
            let r#return = Expr {
                kind: ExprKind::Return(None),
                pos,
                r#type: Some(Type::Unit),
            };
            code_block.exprs.push(r#return);
            return Some(code_block);
        }

        unimplemented!("TBD: parse_code_block");
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

            let code_block = match &cst_body.kind {
                cst::NodeKind::CodeBlock(body) => {
                    match Self::parse_code_block(cst_body.pos, body, errors) {
                        Some(blk) => blk,
                        None => {
                            continue;
                        }
                    }
                }
                _ => {
                    let msg = cst_body.pos.report(format!(
                        "Unexpected function body kind {:?}",
                        &cst_body.kind
                    ));
                    errors.push(msg);
                    continue;
                }
            };

            // At this point all expresions must be parsed
            // We need to make sure return type can be converted to the function return type
            match code_block.r#type() {
                None => {
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
#[path = "_tests/test_ast.rs"] // This path looks like it's for CST tests, not AST tests.
mod test_ast; // Should probably be test_ast
