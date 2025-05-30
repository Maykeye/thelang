use std::collections::HashMap;

use crate::{
    cst::{self, CST, Node},
    tokens::Pos,
};

// TODO: replace with Variable?
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TpFunctionArg {
    pub name: String,
    pub r#type: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TpFunction {
    pub args: Vec<TpFunctionArg>,
    pub return_type: Type,
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
pub struct Variable {
    pub name: String,
    pub r#type: Option<Type>,
    pub is_arg: bool,
}

impl Variable {
    pub fn new<N: Into<String>>(name: N, r#type: Option<Type>, is_arg: bool) -> Self {
        Self {
            name: name.into(),
            r#type,
            is_arg,
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Unit,
    Return(Option<Box<Expr>>),
    CodeBlock(CodeBlock),
    Argument(String),
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

impl Function {
    fn get_args(&self) -> &[TpFunctionArg] {
        &self.r#type.args
    }

    pub fn get_argument_index_by_name(&self, name: &str) -> Option<usize> {
        // TODO: refactor codegen to use index instead of name
        self.r#type.args.iter().position(|arg| arg.name == name)
    }
}

#[derive(Debug)]
pub struct AST {
    pub functions: HashMap<String, Function>,
}

#[derive(Debug)]
struct ScopeTracker {
    /// Keep track of local variables with always unique names
    used_variables: HashMap<String, usize>,
    scopes: Vec<HashMap<String, String>>,
}

impl ScopeTracker {
    fn new() -> Self {
        Self {
            used_variables: Default::default(),
            scopes: vec![],
        }
    }
    fn declare_var(&mut self, name: &str) -> String {
        let new_used = match self.used_variables.get_mut(name) {
            Some(old) => {
                *old += 1;
                *old
            }
            None => {
                self.used_variables.insert(name.to_string(), 0);
                0
            }
        };

        let new_name = if new_used == 0 {
            name.to_string()
        } else {
            format!("{name}${new_used}")
        };

        let scope = self
            .scopes
            .last_mut()
            .expect("declaring variables with no current scopes");
        scope.insert(name.to_string(), new_name.clone());
        new_name
    }

    fn resolve_var(&self, name: &str) -> Option<&String> {
        for scope in self.scopes.iter().rev() {
            if let Some(ident) = scope.get(name) {
                return Some(ident);
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.push(Default::default());
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

struct ScopedVariablesData {
    scopes: ScopeTracker,
    data: HashMap<String, Variable>,
}

impl ScopedVariablesData {
    pub fn new() -> Self {
        Self {
            scopes: ScopeTracker::new(),
            data: HashMap::new(),
        }
    }
}

impl AST {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
        }
    }

    fn parse_type<'a>(&mut self, cst: &'a Node, _errors: &mut Vec<String>) -> Result<Type, ()> {
        match cst.kind {
            cst::NodeKind::Unit => Ok(Type::Unit),
            _ => {
                unimplemented!("TBD: return type NYI");
            }
        }
    }

    fn parse_cst_function_declarations_args(
        &mut self,
        cst_func: &cst::Fn,
        errors: &mut Vec<String>,
    ) -> Vec<TpFunctionArg> {
        let mut args_tp: Vec<TpFunctionArg> = vec![];
        for (i, cst_arg) in cst_func.args.iter().enumerate() {
            // Rename "_" to unique identifier.
            let name = match cst_arg.name.as_str() {
                "_" => format!("$arg${i}"),
                other => other.to_string(),
            };

            // Check argument name overshadowing
            let same_name_exist = args_tp.iter().find(|arg| arg.name == name).is_some();
            if same_name_exist {
                let msg = cst_arg
                    .pos
                    .report(format!("Argument {} already exists", name));
                errors.push(msg);
            }

            // Get type
            let r#type = self
                .parse_type(&cst_arg.r#type, errors)
                .unwrap_or(Type::Unit);

            // And finally put it into function arguments
            args_tp.push(TpFunctionArg { name, r#type });
        }
        args_tp
    }

    fn parse_cst_function_declarations<'a>(
        &mut self,
        cst: &'a CST,
        errors: &mut Vec<String>,
    ) -> HashMap<String, &'a cst::Fn> {
        let mut mappings = HashMap::default();
        // Prepare functions headers
        for cst_func in cst.functions.values() {
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

            let args_tp = self.parse_cst_function_declarations_args(&cst_func, errors);
            // Parse return type
            let func_ret_type = match &cst_func.return_type {
                Some(node) => self.parse_type(node, errors).unwrap_or(Type::Unit),
                None => Type::Unit,
            };

            // Assign function type to the global symbol
            let tp_func = TpFunction {
                args: args_tp,
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

    fn parse_expr(
        cst_expr: &cst::Node,
        vars: &ScopedVariablesData,
        errors: &mut Vec<String>,
    ) -> Result<Expr, ()> {
        match &cst_expr.kind {
            cst::NodeKind::Unit => Ok(Expr::new(ExprKind::Unit, cst_expr.pos, Some(Type::Unit))),
            cst::NodeKind::Identifier(name) => {
                let name = match vars.scopes.resolve_var(name) {
                    Some(name) => name,
                    None => {
                        let msg = cst_expr.pos.report(format!("unknown variable {name}"));
                        errors.push(msg);
                        return Err(());
                    }
                };

                let var_data = vars
                    .data
                    .get(name)
                    .expect("internal error: known variable has no known type");
                let r#type = match &var_data.r#type {
                    Some(tp) => tp.clone(),
                    None => {
                        let msg = cst_expr
                            .pos
                            .report(format!("variable type for {name} is not known"));
                        errors.push(msg);
                        return Err(());
                    }
                };

                let kind = if !var_data.is_arg {
                    unimplemented!("local vars nyi; only args")
                } else {
                    ExprKind::Argument(name.clone())
                };

                Ok(Expr::new(kind, cst_expr.pos, Some(r#type)))
            }
            _ => unimplemented!("nyi"),
        }
    }

    fn parse_code_block(
        cst_cb: &cst::CodeBlock,
        vars: &mut ScopedVariablesData,
        errors: &mut Vec<String>,
    ) -> Option<CodeBlock> {
        vars.scopes.push_scope();
        let data = Self::parse_code_block_impl(cst_cb, vars, errors);
        vars.scopes.pop_scope();
        data
    }

    fn parse_code_block_impl(
        cst_cb: &cst::CodeBlock,
        vars: &mut ScopedVariablesData,
        errors: &mut Vec<String>,
    ) -> Option<CodeBlock> {
        let pos = cst_cb.pos;
        let mut code_block = CodeBlock::new(pos);
        // Empty body = return ()
        if cst_cb.nodes.is_empty() {
            let r#return = Expr::new(ExprKind::Return(None), pos, Some(Type::Never));
            code_block.exprs.push(r#return);
            code_block.return_type = Some(Type::Unit);
            return Some(code_block);
        }

        // Convert CST untyped nodes to AST typed nodes
        for cst_node in cst_cb.nodes.iter() {
            match &cst_node.kind {
                cst::NodeKind::Return(val) => {
                    // Parse conversion
                    let expr = match Self::parse_expr(val, vars, errors) {
                        Ok(expr) => expr,
                        Err(()) => return None,
                    };

                    // If it's the second return type, make sure they are convertible
                    assert!(
                        expr.r#type.is_some(),
                        "{}",
                        cst_node.pos.report("unknown type")
                    );
                    if let Some(current_return_type) = code_block.return_type {
                        let new_type = expr.r#type.as_ref().unwrap();
                        // TODO: return or panic of expt type not yet defined?
                        Self::check_type_implicit_conversion(
                            cst_node.pos,
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

                cst::NodeKind::CodeBlock(cst_nested_cb) => {
                    let new_cb = match Self::parse_code_block(cst_nested_cb, vars, errors) {
                        Some(cb) => cb,
                        None => return None,
                    };

                    let last_pos = new_cb.last_expr_pos();
                    let block_type = new_cb.exprs.last().map_or(Type::Unit, |t| {
                        t.r#type.as_ref().expect("Internal error").clone()
                    });

                    let abort = match (&code_block.return_type, &new_cb.return_type) {
                        (Some(old_type), Some(new_type)) => !Self::check_type_implicit_conversion(
                            last_pos, &new_type, &old_type, errors,
                        ),
                        (None, Some(new_type)) => {
                            code_block.return_type = Some(new_type.clone());
                            false
                        }
                        _ => false,
                    };

                    if abort {
                        return None;
                    }

                    let new_expr = Expr::new(ExprKind::CodeBlock(new_cb), pos, Some(block_type));
                    code_block.exprs.push(new_expr);
                }

                _ => {
                    // TODO: collect errors and return None in the end
                    match Self::parse_expr(cst_node, vars, errors) {
                        Ok(expr) => {
                            code_block.exprs.push(expr);
                        }
                        Err(()) => return None,
                    };
                }
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

            let mut vars = ScopedVariablesData::new();
            vars.scopes.push_scope();
            for arg in func.get_args() {
                let name = vars.scopes.declare_var(&arg.name);
                vars.data.insert(
                    name.clone(),
                    Variable {
                        name: name,
                        r#type: Some(arg.r#type.clone()),
                        is_arg: true,
                    },
                );
            }

            let mut code_block = match Self::parse_code_block(cst_body, &mut vars, errors) {
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
