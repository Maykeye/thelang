use std::collections::HashMap;

use crate::{
    cst::{self, CST, Node},
    tokens::{Pos, TokenKind},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct AstTypeId(pub usize);

impl AstTypeId {
    pub const NEVER: AstTypeId = AstTypeId(0);
    pub const UNIT: AstTypeId = AstTypeId(1);
    pub const BOOL: AstTypeId = AstTypeId(2);
}

// TODO: replace with Variable?
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TpFunctionArg {
    pub name: String,
    pub type_id: AstTypeId,
    pub pos: Pos,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TpFunction {
    pub args: Vec<TpFunctionArg>,
    pub return_type: AstTypeId,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AstType {
    Function(Box<TpFunction>),
    /// `!` is a so called never type, which has no instances
    Never,
    /// Unit type. Has exactly one instance ()
    Unit,
    /// Boolean type: only true or false
    Bool,
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub type_id: Option<AstTypeId>,
    pub is_arg: bool,
}

impl Variable {
    pub fn new<N: Into<String>>(name: N, type_id: Option<AstTypeId>, is_arg: bool) -> Self {
        Self {
            name: name.into(),
            type_id,
            is_arg,
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    /// Unit litreal: ()
    Unit,
    /// Boolean literal: true, false
    BooleanLiteral(bool),
    /// Return from the function
    Return(Option<Box<Expr>>),
    /// Return from the code block
    BlockReturn(Option<Box<Expr>>),
    /// Boolean op: invert. ~x
    Invert(Box<Expr>),
    /// Boolean op: and. x & y
    And(Box<Expr>, Box<Expr>),
    /// Code block
    CodeBlock(CodeBlock),
    /// Usage argument named ARGNAME
    // TODO: replace string name to numeric id
    Argument(String),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub pos: Pos,
    pub type_id: Option<AstTypeId>,
}
impl Expr {
    pub fn new(kind: ExprKind, pos: Pos, type_id: Option<AstTypeId>) -> Self {
        Self { kind, pos, type_id }
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
    pub return_type_id: Option<AstTypeId>,
}

impl CodeBlock {
    fn new(pos: Pos) -> Self {
        Self {
            exprs: Default::default(),
            pos,
            return_type_id: None,
        }
    }

    fn last_expr_pos(&self) -> Pos {
        self.exprs.last().map_or(self.pos, |e| e.pos)
    }
}

#[derive(Debug)]
pub struct Function {
    pub decl_pos: Pos,
    pub name: String,
    pub type_id: TpFunction,
    pub body: Option<CodeBlock>,
}

impl Function {
    fn get_args(&self) -> &[TpFunctionArg] {
        &self.type_id.args
    }

    pub fn get_argument_index_by_name(&self, name: &str) -> Option<usize> {
        // TODO: refactor codegen to use index instead of name
        self.type_id.args.iter().position(|arg| arg.name == name)
    }
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

/// AstErrorContextKind shows in which immediate context the error happened
#[derive(Debug, PartialEq, Eq)]
pub enum AstErrorContextKind {
    FunctionDeclaration,
    BinOp(TokenKind),
    UnaryOp(TokenKind),
    FunctionReturn,
    CodeBlockReturn,
    None,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstErrorContext {
    error_pos: Pos,
    kind: AstErrorContextKind,
}
impl AstErrorContext {
    fn new_unop(pos: Pos, token_kind: TokenKind) -> AstErrorContext {
        AstErrorContext {
            error_pos: pos,
            kind: AstErrorContextKind::UnaryOp(token_kind),
        }
    }
    fn new_binop(pos: Pos, token_kind: TokenKind) -> AstErrorContext {
        AstErrorContext {
            error_pos: pos,
            kind: AstErrorContextKind::BinOp(token_kind),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstError {
    /// fn foo(diff_args_have_same_name:(), diff_args_have_same_name:())
    FunctionArgumentNameDuplicated(AstErrorContext, String, Pos),
    /// fn foo1(differerent_funcs:())
    /// fn foo1(have_the_same_name:())
    FunctionNameDuplicated(AstErrorContext, String, Pos),
    /// "Abc"*"def"
    TypeConversion(AstErrorContext, AstTypeId, AstTypeId),
    /// 1+this_variable_was_not_declared
    UndeclaredVariable(AstErrorContext, String),
    /// let a; return a;
    UndeclaredVariableType(AstErrorContext, String),
    // fn this_func_has_no_body();
    UndefinedFunction(AstErrorContext, String),
}

impl AstError {
    fn new_func_arg_name_duplicated(pos: Pos, arg_name: String, other_pos: Pos) -> AstError {
        let ctx = AstErrorContext {
            error_pos: pos,
            kind: AstErrorContextKind::FunctionDeclaration,
        };
        AstError::FunctionArgumentNameDuplicated(ctx, arg_name, other_pos)
    }
    fn new_func_name_duplicated(pos: Pos, func_name: String, other_pos: Pos) -> AstError {
        let ctx = AstErrorContext {
            error_pos: pos,
            kind: AstErrorContextKind::FunctionDeclaration,
        };
        AstError::FunctionNameDuplicated(ctx, func_name, other_pos)
    }
    fn new_undeclared_variable(pos: Pos, variable: impl Into<String>) -> AstError {
        let ctx = AstErrorContext {
            error_pos: pos,
            kind: AstErrorContextKind::None,
        };
        AstError::UndeclaredVariable(ctx, variable.into())
    }

    fn new_undefined_function(pos: Pos, function_name: impl Into<String>) -> AstError {
        let ctx = AstErrorContext {
            error_pos: pos,
            kind: AstErrorContextKind::FunctionDeclaration,
        };
        AstError::UndefinedFunction(ctx, function_name.into())
    }
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub struct AST {
    pub functions: HashMap<String, Function>,
    pub types: HashMap<AstTypeId, AstType>,
}
impl AST {
    pub fn new() -> Self {
        let mut types = HashMap::new();
        types.insert(AstTypeId::NEVER, AstType::Never);
        types.insert(AstTypeId::UNIT, AstType::Unit);
        types.insert(AstTypeId::BOOL, AstType::Bool);
        Self {
            functions: Default::default(),
            types,
        }
    }

    pub fn get_type(&self, type_id: AstTypeId) -> Option<&AstType> {
        self.types.get(&type_id)
    }

    fn parse_type(&mut self, cst: &Node, _errors: &mut Vec<AstError>) -> Result<AstTypeId, ()> {
        match &cst.kind {
            cst::NodeKind::Unit => Ok(AstTypeId::UNIT),
            cst::NodeKind::Identifier(named_type) => match named_type.as_str() {
                "bool" => Ok(AstTypeId::BOOL),
                _ => unimplemented!(),
            },
            _ => {
                unimplemented!("TBD: return type NYI");
            }
        }
    }

    fn parse_cst_function_declarations_args(
        &mut self,
        cst_func: &cst::Fn,
        errors: &mut Vec<AstError>,
    ) -> Vec<TpFunctionArg> {
        let mut args_tp: Vec<TpFunctionArg> = vec![];
        for (i, cst_arg) in cst_func.args.iter().enumerate() {
            // Rename "_" to unique identifier.
            let name = match cst_arg.name.as_str() {
                "_" => format!("$arg${i}"),
                other => other.to_string(),
            };

            // Check argument name overshadowing
            if let Some(other) = args_tp.iter().find(|arg| arg.name == name) {
                errors.push(AstError::new_func_arg_name_duplicated(
                    cst_arg.pos,
                    name,
                    other.pos,
                ));
                continue;
            }

            // Get type
            let type_id = self
                .parse_type(&cst_arg.r#type, errors)
                .unwrap_or(AstTypeId::UNIT);

            // And finally put it into function arguments
            args_tp.push(TpFunctionArg {
                name,
                type_id,
                pos: cst_arg.pos,
            });
        }
        args_tp
    }

    fn parse_cst_function_declarations<'a>(
        &mut self,
        cst: &'a CST,
        errors: &mut Vec<AstError>,
    ) -> HashMap<String, &'a cst::Fn> {
        let mut mappings = HashMap::default();
        // Prepare functions headers
        for cst_func in cst.functions.values() {
            // Ensure such function doesn't override existing function
            let full_name = cst_func.name.clone();
            if let Some(other) = self.functions.get(&full_name) {
                errors.push(AstError::new_func_name_duplicated(
                    cst_func.pos,
                    full_name,
                    other.decl_pos,
                ));
                continue;
            }

            let args_tp = self.parse_cst_function_declarations_args(cst_func, errors);
            // Parse return type
            let func_ret_type = match &cst_func.return_type {
                Some(node) => self.parse_type(node, errors).unwrap_or(AstTypeId::UNIT),
                None => AstTypeId::UNIT,
            };

            // Assign function type to the global symbol
            let tp_func = TpFunction {
                args: args_tp,
                return_type: func_ret_type,
            };
            let func = Function {
                decl_pos: cst_func.pos,
                name: full_name.clone(),
                type_id: tp_func,
                body: None,
            };

            mappings.insert(full_name.clone(), cst_func);
            self.functions.insert(full_name, func);
        }
        mappings
    }

    fn check_type_implicit_conversion(
        from: AstTypeId,
        to: AstTypeId,
        errors: &mut Vec<AstError>,
        get_ctx: impl Fn() -> AstErrorContext,
    ) -> bool {
        if from == to {
            true
        } else {
            errors.push(AstError::TypeConversion(get_ctx(), from, to));
            false
        }
    }

    /// Parse `true` or `false`
    // TODO: move to cst to not store dozens of strings true/false?
    fn parse_expr_boolean_literal(cst_expr: &cst::Node) -> Option<Expr> {
        if let cst::NodeKind::Identifier(id) = &cst_expr.kind {
            let bool_value = match id.as_str() {
                "true" => Some(true),
                "false" => Some(false),
                _ => None,
            };

            bool_value.map(|b| {
                Expr::new(
                    ExprKind::BooleanLiteral(b),
                    cst_expr.pos,
                    Some(AstTypeId::BOOL),
                )
            })
        } else {
            None
        }
    }

    fn parse_expr(
        cst_expr: &cst::Node,
        vars: &ScopedVariablesData,
        errors: &mut Vec<AstError>,
    ) -> Result<Expr, ()> {
        // check builtin names
        if let Some(expr) = Self::parse_expr_boolean_literal(cst_expr) {
            return Ok(expr);
        }
        match &cst_expr.kind {
            cst::NodeKind::Unit => Ok(Expr::new(
                ExprKind::Unit,
                cst_expr.pos,
                Some(AstTypeId::UNIT),
            )),
            cst::NodeKind::Identifier(name) => {
                // resolve by scope
                let name = match vars.scopes.resolve_var(name) {
                    Some(name) => name,
                    None => {
                        errors.push(AstError::new_undeclared_variable(cst_expr.pos, name));
                        return Err(());
                    }
                };

                let var_data = vars
                    .data
                    .get(name)
                    .expect("internal error: known variable has no known type");
                let type_id = match &var_data.type_id {
                    Some(tp) => *tp,
                    None => {
                        errors.push(AstError::UndeclaredVariableType(
                            AstErrorContext {
                                error_pos: cst_expr.pos,
                                kind: AstErrorContextKind::None,
                            },
                            name.to_string(),
                        ));
                        return Err(());
                    }
                };

                let kind = if !var_data.is_arg {
                    unimplemented!("local vars nyi; only args")
                } else {
                    ExprKind::Argument(name.clone())
                };

                Ok(Expr::new(kind, cst_expr.pos, Some(type_id)))
            }

            cst::NodeKind::And(lhs, rhs) => {
                let lhs = Self::parse_expr(lhs, vars, errors)?;
                if lhs.type_id != Some(AstTypeId::BOOL) {
                    errors.push(AstError::TypeConversion(
                        AstErrorContext::new_binop(lhs.pos, TokenKind::Ampersand),
                        lhs.type_id.unwrap_or(AstTypeId::NEVER),
                        AstTypeId::BOOL,
                    ));
                    return Err(());
                }
                let rhs = Self::parse_expr(rhs, vars, errors)?;
                if rhs.type_id != Some(AstTypeId::BOOL) {
                    errors.push(AstError::TypeConversion(
                        AstErrorContext::new_binop(rhs.pos, TokenKind::Ampersand),
                        rhs.type_id.unwrap_or(AstTypeId::NEVER),
                        AstTypeId::BOOL,
                    ));
                    return Err(());
                }
                let kind = ExprKind::And(Box::new(lhs), Box::new(rhs));
                Ok(Expr::new(kind, cst_expr.pos, Some(AstTypeId::BOOL)))
            }

            cst::NodeKind::Invert(inner) => {
                let inner = Self::parse_expr(inner, vars, errors)?;
                if inner.type_id != Some(AstTypeId::BOOL) {
                    errors.push(AstError::TypeConversion(
                        AstErrorContext::new_unop(cst_expr.pos, TokenKind::Exclamation),
                        inner.type_id.unwrap_or(AstTypeId::NEVER),
                        AstTypeId::BOOL,
                    ));
                    return Err(());
                }
                let kind = ExprKind::Invert(Box::new(inner));
                Ok(Expr::new(kind, cst_expr.pos, Some(AstTypeId::BOOL)))
            }
            _ => unimplemented!("nyi"),
        }
    }

    fn parse_code_block(
        cst_cb: &cst::CodeBlock,
        vars: &mut ScopedVariablesData,
        errors: &mut Vec<AstError>,
    ) -> Option<CodeBlock> {
        vars.scopes.push_scope();
        let data = Self::parse_code_block_impl(cst_cb, vars, errors);
        vars.scopes.pop_scope();
        data
    }

    fn parse_code_block_impl(
        cst_cb: &cst::CodeBlock,
        vars: &mut ScopedVariablesData,
        errors: &mut Vec<AstError>,
    ) -> Option<CodeBlock> {
        let pos = cst_cb.pos;
        let mut code_block = CodeBlock::new(pos);
        // Empty body = return ()
        if cst_cb.nodes.is_empty() {
            let r#return = Expr::new(ExprKind::BlockReturn(None), pos, Some(AstTypeId::NEVER));
            code_block.exprs.push(r#return);
            code_block.return_type_id = Some(AstTypeId::UNIT);
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
                        expr.type_id.is_some(),
                        "{}",
                        cst_node.pos.report("unknown type")
                    );
                    if let Some(current_return_type) = code_block.return_type_id {
                        let new_type = expr.type_id.as_ref().unwrap();
                        // TODO: return or panic of expt type not yet defined?
                        Self::check_type_implicit_conversion(
                            *new_type,
                            current_return_type,
                            errors,
                            || AstErrorContext {
                                error_pos: cst_node.pos,
                                kind: AstErrorContextKind::FunctionReturn,
                            },
                        );
                    }

                    // Return
                    code_block.return_type_id = expr.type_id;
                    let expr = Expr::new(
                        ExprKind::BlockReturn(Some(Box::new(expr))),
                        pos,
                        Some(AstTypeId::NEVER),
                    );
                    code_block.exprs.push(expr);
                }

                cst::NodeKind::CodeBlock(cst_nested_cb) => {
                    let new_cb = Self::parse_code_block(cst_nested_cb, vars, errors)?;
                    let block_type = new_cb.exprs.last().map_or(AstTypeId::UNIT, |t| {
                        *t.type_id.as_ref().expect("Internal error")
                    });

                    let abort = match (&code_block.return_type_id, &new_cb.return_type_id) {
                        (Some(old_type), Some(new_type)) => !Self::check_type_implicit_conversion(
                            *new_type,
                            *old_type,
                            errors,
                            || AstErrorContext {
                                error_pos: cst_nested_cb.pos,
                                kind: AstErrorContextKind::CodeBlockReturn,
                            },
                        ),
                        (None, Some(new_type)) => {
                            code_block.return_type_id = Some(*new_type);
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

        Some(code_block)
    }

    fn parse_cst_function_definition(
        &mut self,
        errors: &mut Vec<AstError>,
        mappings: &HashMap<String, &cst::Fn>,
    ) {
        for func in self.functions.values_mut() {
            let cst_func = *mappings.get(&func.name).unwrap_or_else(|| {
                panic!("Internal error: can't find the CST body of {}", &func.name)
            });

            let cst_body = match &cst_func.body {
                Some(body) => body,
                None => {
                    errors.push(AstError::new_undefined_function(func.decl_pos, &func.name));
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
                        name,
                        type_id: Some(arg.type_id),
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
            if code_block.return_type_id.is_none() {
                let tp = match code_block.exprs.last() {
                    Some(e) => {
                        let t = match e.type_id.as_ref() {
                            Some(t) => *t,
                            None => {
                                let msg =  code_block.pos.report( "internal error: code block last expression type is not resolved");
                                panic!("{}", msg)
                            }
                        };
                        t
                    }
                    None => AstTypeId::UNIT,
                };
                code_block.return_type_id = Some(tp);
            }

            // At this point all expresions must be parsed
            // We need to make sure return type can be converted to the function return type
            match code_block.return_type_id.as_ref() {
                None => {
                    panic!("internal error: no type for function body found");
                }

                Some(type_id) => {
                    Self::check_type_implicit_conversion(
                        *type_id,
                        func.type_id.return_type,
                        errors,
                        || AstErrorContext {
                            error_pos: code_block.last_expr_pos(),
                            kind: AstErrorContextKind::FunctionReturn,
                        },
                    );
                }
            }

            func.body = Some(code_block);
        }
    }

    pub fn from_cst(cst: CST) -> Result<AST, (AST, Vec<AstError>)> {
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

// Test stuff
#[cfg(test)]
#[path = "_tests/test_ast.rs"]
mod test_ast;
#[cfg(test)]
#[path = "_tests/test_ast_expr.rs"]
mod test_ast_expr;
