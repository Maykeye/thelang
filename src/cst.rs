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
    name: String,
    pos: Pos,
    body: Vec<Node>,
    args: Vec<Arg>,
    return_type: Option<Box<Node>>,
}

impl Fn {
    pub fn new(name: String, pos: Pos) -> Self {
        Self {
            name,
            pos,
            body: vec![],
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

/// Concrete syntax tree nodes
#[derive(Debug)]
pub enum NodeKind {
    Fn(Fn),
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

    pub fn parse_fn(toks: &Tokens, index: &mut usize) -> Result<Fn, (usize, String)> {
        let mut i = *index;
        let i0 = i;

        // FN
        if !toks.kind_eq(i, TokenKind::Fn) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err((i, "function declaration: expected fn".to_string()));
        }
        i += 1;

        // <ident>
        let name = match toks.get_identifier(i) {
            Some(name) => name,
            None => {
                *index = Self::error_recovery_find_completed_block(toks, i);
                return Err((i, "identifier(function name) expected".to_string()));
            }
        };
        i += 1;

        // Args
        if !toks.kind_eq(i, TokenKind::LParen) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err((i, "function declaration: arguments expected".to_string()));
        }
        i += 1;

        // )
        if !toks.kind_eq(i, TokenKind::RParen) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err((
                i,
                "function declaration: end of arguments expected".to_string(),
            ));
        }
        i += 1;

        // {
        if !toks.kind_eq(i, TokenKind::LCurly) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err((i, "function definition: code block expected".to_string()));
        }
        i += 1;

        // }
        if !toks.kind_eq(i, TokenKind::RCurly) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err((i, "function definition: end of block expected".to_string()));
        }
        i += 1;

        *index = i;
        Ok(Fn::new(name, toks[i0].pos))
    }

    pub fn from_tokens(tokens: &[Token]) -> Result<CST, (CST, Vec<String>)> {
        let toks = Tokens::new(tokens);
        let mut i = 0;
        let mut cst = CST::new();
        let mut errors = vec![];
        let mut add_error = |idx: usize, msg| {
            errors.push(toks[idx.min(toks.len() - 1)].pos.report(msg));
        };

        while i < toks.len() {
            let i0 = i;
            match toks[i].kind {
                TokenKind::Fn => {
                    let func = match Self::parse_fn(&toks, &mut i) {
                        Ok(func) => func,
                        Err((err_pos, err_msg)) => {
                            assert!(i > i0, "internal error: parser stuck at fn parsing");
                            add_error(err_pos, err_msg);
                            continue;
                        }
                    };
                    assert!(i > i0, "internal error: parser stuck at fn parsing");

                    if let Some(msg) = cst.chkerr_check_name_duplicate(&cst.functions, &func.name) {
                        add_error(i0, msg);
                        continue;
                    }

                    cst.functions.insert(func.name.clone(), func);
                }
                _ => {
                    add_error(i, format!("Unexpected top-level token {:?}", toks[i].kind));
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
