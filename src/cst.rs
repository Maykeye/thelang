use std::collections::HashMap;

use crate::tokens::{Pos, Token, TokenKind, Tokens};

/// Concrete syntax tree: functions
#[derive(Debug)]
pub struct Arg {
    name: String,
    r#type: Node,
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
}

#[derive(Debug)]
pub struct CST {
    pub functions: HashMap<String, Fn>,
}

impl CST {
    pub fn new() -> Self {
        CST {
            functions: Default::default(),
        }
    }

    /// Error recovery: if something that must preceed {} had error, find the start of the
    /// block and its end taking nested blocks into account and skip the content of the block
    /// We'll treat `foobar() }` as foobar(){}
    pub fn error_recovery_find_completed_block(toks: &Tokens, mut i: usize) -> usize {
        // Skip the next block
        while i < toks.len() {
            if toks[i].kind == TokenKind::LCurly {
                i += 1;
                break;
            }
            if toks[i].kind == TokenKind::RCurly {
                return i + 1;
            }
            i += 1;
        }
        // no block found
        if i >= toks.len() {
            return i;
        }

        // Block found
        let mut level = 1;
        while i < toks.len() {
            if toks[i].kind == TokenKind::LCurly {
                level += 1;
            }
            if toks[i].kind == TokenKind::RCurly {
                level -= 1;
                if level == 0 {
                    i += 1;
                    break;
                }
            }
            i += 1;
        }
        i
    }

    fn chkerr_check_name_duplicate<V: GetPos>(
        &self,
        map: &HashMap<String, V>,
        name: &str,
    ) -> Option<String> {
        if let Some(other) = map.get(name) {
            return Some(format!(
                "Duplicate of the name {}, previously the name was used at {:?}",
                name,
                other.get_pos()
            ));
        } else {
            None
        }
    }

    pub fn parse_expr(toks: &Tokens, mut i: usize) -> (usize, Result<Node, String>) {
        // TBD: RDP over exprs
        if toks.kind_eq(i, TokenKind::LParen) && toks.kind_eq(i + 1, TokenKind::RParen) {
            i += 2;
            return (i, Ok(Node::new(NodeKind::Unit, toks[i - 2].pos)));
        }

        return (
            i,
            Err(format!(
                "unknown expr kind {:?}",
                toks.get_nth_kind_description(i)
            )),
        );
    }

    pub fn parse_code_block(toks: &Tokens, mut i: usize) -> (usize, Result<CodeBlock, String>) {
        // {
        let mut cb = CodeBlock::new(toks[i].pos);
        if !toks.kind_eq(i, TokenKind::LCurly) {
            i = Self::error_recovery_find_completed_block(toks, i);
            return (
                i,
                Err("function definition: code block expected".to_string()),
            );
        }
        i += 1;

        while i < toks.len() {
            match toks[i].kind {
                TokenKind::Semi => {
                    // Semicolon is a discarding nop operator. It gets replced by () node.
                    // TODO: consume not doing it unless it's the end of the block
                    cb.nodes.push(Node::new(NodeKind::Unit, toks[i].pos));
                    i += 1;
                    continue;
                }

                TokenKind::RCurly => {
                    break;
                }

                TokenKind::Return => {
                    let ret_pos = toks[i].pos;
                    i += 1;

                    // `return ();` synonyms:
                    // * return ;
                    // * return }
                    if toks.kind_eq(i, TokenKind::Semi) || toks.kind_eq(i, TokenKind::RCurly) {
                        // We consume `return` and leave parsing of the remaining token to the main
                        // loop
                        let unit = Node::new(NodeKind::Unit, ret_pos);
                        cb.nodes
                            .push(Node::new(NodeKind::Return(Box::new(unit)), ret_pos));
                        continue;
                    }

                    let parsed_expr = Self::parse_expr(toks, i);
                    i = parsed_expr.0;
                    let expr = match parsed_expr.1 {
                        Ok(expr) => expr,
                        Err(msg) => return (i, Err(msg)),
                    };

                    // return expression has 2 forms
                    // * return <expr> ;
                    // * return <expr> }
                    // If we parseed the expression, but the next
                    let ok = toks.kind_eq(i, TokenKind::Semi) || toks.kind_eq(i, TokenKind::RCurly);
                    if !ok {
                        let msg = format!(
                            "after `return` expression only acceptable tokens are `;` and `}}`, got {}",
                            toks.get_nth_kind_description(i)
                        );
                        return (i, Err(toks.get_nth_pos(i).report(msg.to_string())));
                    }

                    cb.nodes
                        .push(Node::new(NodeKind::Return(Box::new(expr)), ret_pos));
                    continue;
                }
                _ => {
                    return (
                        i,
                        Err(format!(
                            "function definition: unknown token: {:?}",
                            toks[i].kind
                        )),
                    );
                }
            }
        }

        // }
        if !toks.kind_eq(i, TokenKind::RCurly) {
            i = Self::error_recovery_find_completed_block(toks, i);
            return (
                i,
                Err("function definition: end of block expected".to_string()),
            );
        }
        // End of the function definition
        (i + 1, Ok(cb))
    }

    pub fn parse_fn(toks: &Tokens, mut i: usize) -> (usize, Result<Fn, String>) {
        let i0 = i;

        // FN
        if !toks.kind_eq(i, TokenKind::Fn) {
            i = Self::error_recovery_find_completed_block(toks, i);
            return (i, Err("function declaration: expected fn".to_string()));
        }
        i += 1;

        // <ident>
        let name = match toks.get_identifier(i) {
            Some(name) => name,
            None => {
                i = Self::error_recovery_find_completed_block(toks, i);
                return (i, Err("identifier(function name) expected".to_string()));
            }
        };
        i += 1;

        // Args
        if !toks.kind_eq(i, TokenKind::LParen) {
            i = Self::error_recovery_find_completed_block(toks, i);
            return (
                i,
                Err("function declaration: arguments expected".to_string()),
            );
        }
        i += 1;

        // )
        if !toks.kind_eq(i, TokenKind::RParen) {
            i = Self::error_recovery_find_completed_block(toks, i);
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

        let mut func = Fn::new(name, toks[i0].pos);

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

                    if let Some(msg) = cst.chkerr_check_name_duplicate(&cst.functions, &func.name) {
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
                    i = Self::error_recovery_find_completed_block(&toks, i);
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

#[cfg(test)]
#[path = "_tests/test_cst.rs"]
mod test_cst;
