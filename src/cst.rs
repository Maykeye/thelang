use std::collections::HashMap;

use crate::tokens::{Pos, Token, TokenKind, Tokens};

/// Concrete syntax tree: functions
#[derive(Debug)]
pub struct Arg {
    pub name: String,
    pub r#type: Node,
    pub pos: Pos,
}
impl Arg {
    fn new<N: Into<String>>(name: N, r#type: Node, pos: Pos) -> Self {
        Arg {
            name: name.into(),
            r#type,
            pos,
        }
    }
}

trait GetPos {
    fn get_pos(&self) -> Pos;
}

#[derive(Debug)]
pub struct Fn {
    pub name: String,
    pub pos: Pos,

    // should be Some(CodeBlock) for existing code, None for extern
    // funcs without codes
    pub body: Option<Box<CodeBlock>>,
    pub args: Vec<Arg>,
    pub return_type: Option<Box<Node>>,
}

impl Fn {
    pub fn new(name: String, pos: Pos) -> Self {
        Self {
            name,
            pos,
            body: None,
            args: vec![],
            return_type: None,
        }
    }
}

impl GetPos for Fn {
    fn get_pos(&self) -> Pos {
        self.pos
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    pub nodes: Vec<Node>,
    pub pos: Pos,
}

impl CodeBlock {
    fn new(pos: Pos) -> Self {
        Self {
            nodes: Default::default(),
            pos,
        }
    }
}

/// Concrete syntax tree nodes
#[derive(Debug)]
pub enum NodeKind {
    Fn(Fn),
    CodeBlock(CodeBlock),
    Return(Box<Node>),
    Invert(Box<Node>),
    Identifier(String),
    Unit,
}

#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub pos: Pos,
}

impl Node {
    fn new(kind: NodeKind, pos: Pos) -> Self {
        Self { kind, pos }
    }

    fn new_identifier<S: Into<String>>(identifier: S, pos: Pos) -> Node {
        Self {
            kind: NodeKind::Identifier(identifier.into()),
            pos,
        }
    }

    pub fn new_unit(pos: Pos) -> Node {
        Self {
            kind: NodeKind::Unit,
            pos,
        }
    }

    pub fn new_invert(inner: Node, pos: Pos) -> Node {
        Self {
            kind: NodeKind::Invert(Box::new(inner)),
            pos,
        }
    }

    pub fn new_return(value: Option<Node>, pos: Pos) -> Node {
        let expr = match value {
            Some(expr) => expr,
            None => Node::new_unit(pos),
        };
        Self {
            kind: NodeKind::Return(Box::new(expr)),
            pos,
        }
    }
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub struct CST {
    pub functions: HashMap<String, Fn>,
}

type OptExprParsingResult = Option<(usize, Result<Node, String>)>;

impl CST {
    pub fn new() -> Self {
        CST {
            functions: Default::default(),
        }
    }

    /// Find a token that must separate exprssion from other and return its position.
    /// It means we are looking for the next semicolon or RCurly as they 100% end the whole
    /// expression. Nested expressions are skipped
    /// Returned position: position of the separator
    pub fn error_recovery_find_expr_end(toks: &Tokens, mut i: usize) -> usize {
        let mut brace_level = 0;

        while i < toks.len() {
            match toks[i].kind {
                TokenKind::RCurly if brace_level == 0 => return i,
                TokenKind::Semi if brace_level == 0 => return i,
                TokenKind::RCurly => {
                    brace_level -= 1;
                }
                TokenKind::LCurly => {
                    brace_level += 1;
                }
                _ => (),
            }

            i += 1;
        }
        i
    }
    /// Find a block termination
    /// It means we are looking for the next RCurly as it 100% ends
    /// block. Nested blocks are skipped
    /// Returned position: position of the separator
    pub fn error_recovery_find_current_block_end(toks: &Tokens, mut i: usize) -> usize {
        let mut brace_level = 0;

        while i < toks.len() {
            match toks[i].kind {
                TokenKind::RCurly if brace_level == 0 => return i,
                TokenKind::Semi if brace_level == 0 => return i,
                TokenKind::RCurly => {
                    brace_level -= 1;
                }
                TokenKind::LCurly => {
                    brace_level += 1;
                }
                _ => (),
            }

            i += 1;
        }
        i
    }
    /// Find a block start, then termination
    /// It means we are looking for the next RCurly as it 100% ends
    /// block. Nested blocks are skipped
    /// Returned position: position of the separator
    pub fn error_recovery_find_next_block_end(toks: &Tokens, mut i: usize) -> usize {
        let mut brace_level = -1;

        while i < toks.len() {
            match toks[i].kind {
                TokenKind::RCurly if brace_level <= 0 => return i,
                TokenKind::Semi if brace_level == 0 => return i,
                TokenKind::RCurly => {
                    brace_level -= 1;
                }
                TokenKind::LCurly => {
                    brace_level += 1;
                }
                _ => (),
            }

            i += 1;
        }
        i
    }

    fn chkerr_check_name_duplicate<V: GetPos>(
        &self,
        map: &HashMap<String, V>,
        name: &str,
    ) -> Result<(), String> {
        if let Some(other) = map.get(name) {
            Err(format!(
                "Duplicate of the name {}, previously the name was used at {:?}",
                name,
                other.get_pos()
            ))
        } else {
            Ok(())
        }
    }

    // Check expression separator.
    // If token doesn't terminate the expression(which is semicolon or RCurly),
    // add an error
    pub fn check_expression_separator(toks: &Tokens, index: &mut usize) -> Result<(), String> {
        let mut i = *index;
        // now we need to check separator: `;` or `}`
        if toks.kind_eq(i, TokenKind::Semi) || toks.kind_eq(i, TokenKind::RCurly) {
            return Ok(());
        }
        let msg = format!(
            "after expression only acceptable tokens are `;` and `}}`, got {}",
            toks.get_nth_kind_description(i)
        );

        while i < toks.len() {
            match toks[i].kind {
                TokenKind::Semi | TokenKind::RCurly => {
                    break;
                }
                _ => {
                    i += 1;
                }
            }
        }
        *index = i;
        Err(msg)
    }

    /// Parse terminal expression, if curent token looks like it begins it.
    /// Error is reported only if we parsed
    /// EXPR_TERM ::= ()
    ///           |   IDENT
    ///           |   '{' CODE_BLOCK '}'
    // TODO: make nomal return type
    fn parse_expr_term(toks: &Tokens, mut i: usize) -> OptExprParsingResult {
        // ()
        if toks.kind_eq(i, TokenKind::LParen) && toks.kind_eq(i + 1, TokenKind::RParen) {
            return Some((i + 2, Ok(Node::new_unit(toks[i].pos))));
        }

        // ( EXPR )
        if toks.kind_eq(i, TokenKind::LParen) {
            let start_pos = toks.get_nth_pos(i);
            i += 1;
            let parsed_expr = Self::parse_expr_required(toks, i);
            i = parsed_expr.0;
            let expr = match parsed_expr.1 {
                Ok(expr) => expr,
                Err(msg) => return Some((i, Err(msg))),
            };
            if toks.kind_eq(i, TokenKind::RParen) {
                return Some((i + 1, Ok(expr)));
            }
            let end_pos = toks.get_nth_pos(i);
            let msg = end_pos.report(format!("RParen expected for closing LParen at {start_pos}"));
            return Some((i, Err(msg)));
        }

        // IDENT
        if let Some(ident) = toks.get_identifier(i) {
            let node = Node::new_identifier(ident, toks[i].pos);
            return Some((i + 1, Ok(node)));
        }

        // LCurly. Nested code-block.
        if toks.kind_eq(i, TokenKind::LCurly) {
            let start_pos = toks[i].pos;
            let (next_i, result) = Self::parse_code_block(toks, i);
            i = next_i;
            let nested_block = match result {
                Ok(block) => block,
                Err(err) => {
                    return Some((i, Err(err)));
                }
            };
            let node = Node::new(NodeKind::CodeBlock(nested_block), start_pos);
            return Some((i, Ok(node)));
        }
        None
    }

    // TODO - make normal enum for errors
    fn errmsg_expression_required(toks: &Tokens, i: usize) -> OptExprParsingResult {
        let msg = toks.get_nth_pos(i).report(format!(
            "expression required, found instead `{}`",
            toks.get_nth_kind_description(i)
        ));
        Some((i, Err(msg)))
    }

    /// Parse an unary expression
    /// EXPR_UNARY ::= `return` [EXPR]  
    /// | `!` EXPR_TERM
    /// | EXPR_TERM
    fn parse_expr_unary(toks: &Tokens, i: usize) -> OptExprParsingResult {
        let pos = toks.get_nth_pos(i);
        if toks.kind_eq(i, TokenKind::Return) {
            let return_value = Self::parse_expr_opt(toks, i + 1);
            let (i, return_stmt) = match return_value {
                Some((i, Ok(node))) => (i, Node::new_return(Some(node), pos)),
                Some((i, Err(msg))) => return Some((i, Err(msg))),
                None => (i + 1, Node::new_return(None, pos)),
            };

            return Some((i, Ok(return_stmt)));
        }

        if toks.kind_eq(i, TokenKind::Exclamation) {
            let (i, base) = if let Some(i_base) = Self::parse_expr_term(toks, i + 1) {
                i_base
            } else {
                return Self::errmsg_expression_required(toks, i + 1);
            };

            let inverted = base.map(|node| Node::new_invert(node, pos));

            return Some((i, inverted));
        }

        Self::parse_expr_term(toks, i)
    }

    /// Parse an expression
    /// EXPR ::= EXPR_UNARY
    fn parse_expr_opt(toks: &Tokens, i: usize) -> OptExprParsingResult {
        Self::parse_expr_unary(toks, i)
    }

    /// Wrapper around `parse_expr_opt`, but the expression is required
    fn parse_expr_required(toks: &Tokens, i: usize) -> (usize, Result<Node, String>) {
        let parsed = Self::parse_expr_opt(toks, i);
        match parsed {
            Some(parsed) => parsed,
            None => {
                return Self::errmsg_expression_required(toks, i).unwrap();
            }
        }
    }

    /// Parses code-block
    /// CODE_BLOCK ::= `{` {`;`} [EXPR {`;` {`;`} EXPR} {`;`}] `}`
    /// Returns position after the code block, i.e after '}'
    // TODO: use errors: vec<string>
    fn parse_code_block(toks: &Tokens, mut i: usize) -> (usize, Result<CodeBlock, String>) {
        let mut cb = CodeBlock::new(toks[i].pos);
        if !toks.kind_eq(i, TokenKind::LCurly) {
            i = Self::error_recovery_find_current_block_end(toks, i);
            if toks.kind_eq(i, TokenKind::RCurly) {
                i += 1;
            }
            return (
                i,
                Err("function definition: code block expected".to_string()),
            );
        }
        i += 1;

        while i < toks.len() {
            // Handle expression separators
            match toks[i].kind {
                // Cdoe block terminator
                TokenKind::RCurly => {
                    break;
                }
                // Expression separator. For now it's being replaced with (unit) instance
                TokenKind::Semi => {
                    i += 1;
                    // Semicolon is a discarding nop operator. It gets replced by () node,
                    // but if and only if it's the last node before RCurly
                    if toks.kind_eq(i, TokenKind::RCurly) {
                        cb.nodes.push(Node::new(NodeKind::Unit, toks[i].pos));
                    }
                    continue;
                }

                _ => {
                    let parsed = Self::parse_expr_required(toks, i);
                    let expr = match parsed.1 {
                        Ok(expr) => expr,
                        Err(msg) => {
                            // TODO: use error_recovery_find_expr_end and vec<err>
                            i = Self::error_recovery_find_current_block_end(toks, i);
                            if toks.kind_eq(i, TokenKind::RCurly) {
                                i += 1;
                            }
                            return (i, Err(msg));
                        }
                    };
                    cb.nodes.push(expr);
                    i = parsed.0;
                    // TODO: adapt this function to use vec[string]
                    if let Err(msg) = Self::check_expression_separator(toks, &mut i) {
                        return (i, Err(msg));
                    }
                }
            }
        }

        // }
        if !toks.kind_eq(i, TokenKind::RCurly) {
            i = Self::error_recovery_find_current_block_end(toks, i);
            if toks.kind_eq(i, TokenKind::RCurly) {
                i += 1;
            }
            return (
                i,
                Err("function definition: end of block expected".to_string()),
            );
        }
        // End of the function definition
        (i + 1, Ok(cb))
    }

    pub fn parse_type(toks: &Tokens, i: usize) -> (usize, Result<Node, String>) {
        if toks.kind_eq(i, TokenKind::LParen) && toks.kind_eq(i + 1, TokenKind::RParen) {
            return (i + 2, Ok(Node::new(NodeKind::Unit, toks[i].pos)));
        }

        (
            i + 1,
            if let Some(ident) = toks.get_identifier(i) {
                Ok(Node::new(NodeKind::Identifier(ident), toks[i].pos))
            } else {
                Err(toks.get_nth_pos(i).report(format!(
                    "Type not implemented beyond simlpest; found token: {}",
                    toks.get_nth_kind_description(i)
                )))
            },
        )
    }

    /// Parses function:arguments)
    /// Returns position of RParen,
    /// or first index where it has no idea what to do
    pub fn parse_fn_decl_arguments(
        toks: &Tokens,
        mut i: usize,
        args: &mut Vec<Arg>,
    ) -> (usize, Result<(), String>) {
        if toks.kind_eq(i, TokenKind::RParen) {
            return (i, Ok(()));
        }
        while i < toks.len() {
            // IDENT : TYPE
            if let TokenKind::Identifier(ident) = &toks[i].kind {
                let argument_pos = toks[i].pos;
                i += 1;
                if !toks.kind_eq(i, TokenKind::Colon) {
                    let msg = toks
                        .get_nth_pos(i)
                        .report("function declaration: arguments parsing: colon expected");
                    return (i, Err(msg));
                }
                i += 1;

                let arg_type;
                (i, arg_type) = Self::parse_type(toks, i);
                let arg_type = match arg_type {
                    Ok(arg_type) => arg_type,
                    Err(err) => return (i, Err(err)),
                };
                args.push(Arg::new(ident, arg_type, argument_pos));
                if toks[i].kind == TokenKind::Comma {
                    i += 1;
                }
                if toks[i].kind == TokenKind::RParen {
                    return (i, Ok(()));
                }
                continue;
            }
            let msg = toks.get_nth_pos(i).report("argument expected");
            return (i, Err(msg));
        }

        let msg = toks
            .get_nth_pos(i)
            .report("function declaration: arguments parsing: EOF met before RParen");
        (i, Err(msg))
    }

    pub fn parse_fn(toks: &Tokens, mut i: usize) -> (usize, Result<Fn, String>) {
        let i0 = i;

        // FN
        if !toks.kind_eq(i, TokenKind::Fn) {
            i = Self::error_recovery_find_next_block_end(toks, i);
            if toks.kind_eq(i, TokenKind::RCurly) {
                i += 1;
            }
            return (i, Err("function declaration: expected fn".to_string()));
        }
        i += 1;

        // <ident>
        let name = match toks.get_identifier(i) {
            Some(name) => name,
            None => {
                i = Self::error_recovery_find_next_block_end(toks, i);
                if toks.kind_eq(i, TokenKind::RCurly) {
                    i += 1;
                }
                return (i, Err("identifier(function name) expected".to_string()));
            }
        };
        i += 1;
        let mut func = Fn::new(name, toks[i0].pos);

        // (Args
        if !toks.kind_eq(i, TokenKind::LParen) {
            i = Self::error_recovery_find_next_block_end(toks, i);
            if toks.kind_eq(i, TokenKind::RCurly) {
                i += 1;
            }
            return (
                i,
                Err("function declaration: arguments expected".to_string()),
            );
        }
        i += 1;

        let status;
        (i, status) = Self::parse_fn_decl_arguments(toks, i, &mut func.args);
        if let Err(err) = status {
            i = Self::error_recovery_find_next_block_end(toks, i);
            if toks.kind_eq(i, TokenKind::RCurly) {
                i += 1;
            }
            return (i, Err(err));
        }

        // )
        if !toks.kind_eq(i, TokenKind::RParen) {
            i = Self::error_recovery_find_next_block_end(toks, i);
            if toks.kind_eq(i, TokenKind::RCurly) {
                i += 1;
            }
            return (
                i,
                Err("function declaration: end of arguments expected".to_string()),
            );
        }
        i += 1;

        // TODO: externs
        let parsed_code_block = Self::parse_code_block(toks, i);
        i = parsed_code_block.0;
        let code_block = match parsed_code_block.1 {
            Ok(block) => block,
            Err(msg) => {
                return (i, Err(msg));
            }
        };

        func.body = Some(Box::new(code_block));

        (i, Ok(func))
    }

    pub fn from_tokens(tokens: &[Token]) -> Result<CST, (CST, Vec<String>)> {
        let toks = Tokens::new(tokens);
        let mut i = 0;
        let mut cst = CST::new();
        let mut errors = vec![];

        while i < toks.len() {
            let i0 = i;
            match toks[i].kind {
                TokenKind::Fn => {
                    let parsed_func = Self::parse_fn(&toks, i);
                    i = parsed_func.0;
                    let func = match parsed_func.1 {
                        Ok(func) => func,
                        Err(err_msg) => {
                            assert!(i > i0, "internal error: parser stuck at fn parsing");
                            errors.push(err_msg);
                            continue;
                        }
                    };
                    assert!(i > i0, "internal error: parser stuck at fn parsing");

                    if let Err(msg) = cst.chkerr_check_name_duplicate(&cst.functions, &func.name) {
                        errors.push(toks[i0].pos.report(msg));
                        continue;
                    }

                    cst.functions.insert(func.name.clone(), func);
                }
                _ => {
                    errors.push(
                        toks[i0]
                            .pos
                            .report(format!("Unexpected top-level token {:?}", toks[i].kind)),
                    );
                    i = Self::error_recovery_find_next_block_end(&toks, i);
                    if toks.kind_eq(i, TokenKind::RCurly) {
                        i += 1;
                    }
                    assert!(i > i0, "internal error: parser stuck at error recovering");
                }
            }
        }

        if errors.is_empty() {
            Ok(cst)
        } else {
            Err((cst, errors))
        }
    }
}

// Test stuff
macro_rules! unwrap_cst_kind {
    ($expr:expr, $variant:path) => {
        match $expr {
            $variant(val) => val,
            _ => unwrap_cst_kind!(@ UNEXPECTED_KIND $expr, $variant)
        }
    };

    ($expr:expr, $variant:path, ()) => {
        match $expr {
            $variant => (),
            _ => unwrap_cst_kind!(@ UNEXPECTED_KIND $expr, $variant)
        }
    };

    (@ UNEXPECTED_KIND $expr:expr, $variant:path) => {
        panic!(
            "Expected variant {} but got NodeKind::{:?}",
            stringify!($variant),
            $expr
        )
    }
}

#[cfg(test)]
#[path = "_tests/test_cst.rs"]
mod test_cst;

#[cfg(test)]
#[path = "_tests/test_cst_expr.rs"]
mod test_cst_expr;
