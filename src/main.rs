use std::{collections::HashMap, fmt::Display, iter::Peekable, ops::Index, str::Chars};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Fn,
    Identifier(String),
    LParen,
    RParen,
    LCurly,
    RCurly,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: Pos,
}

impl Token {
    pub fn new(kind: TokenKind, pos: Pos) -> Self {
        Token { kind, pos }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
    pub offset: usize,
}

impl Pos {
    pub fn new(line: usize, col: usize, offset: usize) -> Self {
        Pos { line, col, offset }
    }

    pub fn report<D: Display>(&self, msg: D) -> String {
        format!("{}: {}: {}", self.line, self.col, msg)
    }
}
impl Default for Pos {
    fn default() -> Self {
        Pos::new(1, 1, 0)
    }
}

#[derive(Debug, Clone)]
pub struct CharPos<'a> {
    pub pos: Pos,
    pub it: Peekable<Chars<'a>>,
}
impl<'a> CharPos<'a> {
    pub fn from_str(txt: &'a str) -> Self {
        Self {
            pos: Pos::default(),
            it: txt.chars().peekable(),
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        return self.it.peek().copied();
    }

    pub fn peekz(&mut self) -> char {
        return self.peek().unwrap_or('\0');
    }

    /// Advances one character forward, updating the position
    /// Returns:
    /// * true iff end-of-file was reached
    pub fn advance(&mut self) -> bool {
        match self.peek() {
            None => return true,
            Some('\n') => {
                self.pos.line += 1;
                self.pos.col = 1;
                self.pos.offset += 1;
            }
            _ => {
                self.pos.col += 1;
                self.pos.offset += 1;
            }
        }
        self.it.next();
        self.peek().is_none()
    }

    pub fn consume2(&mut self, ch1: char, ch2: char) -> bool {
        let mut next = self.clone();
        next.advance();
        if self.peek() == Some(ch1) && next.peek() == Some(ch2) {
            self.advance();
            self.advance();
            return true;
        }
        false
    }
}

/// Skip whitespace(//comment line included into whitespace)

fn skip_ws<'a>(cur: &mut CharPos<'a>) {
    loop {
        // Skip white-spaces
        while cur.peekz().is_whitespace() {
            cur.advance();
        }

        // End on non-ws non-comment
        if !cur.consume2('/', '/') {
            break;
        }
        // Skip until the end of the line
        while let Some(c) = cur.peek() {
            if c == '\n' {
                // And skip EOL
                cur.advance();
                break;
            }
            cur.advance();
        }
    }
}

/// Check if character starts an identifier
fn is_id_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

/// Check if character continues an identifier
fn is_id_body(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '$' || c.is_numeric()
}

/// Read identifier
fn read_keyword_or_identifier<'a>(cur: &mut CharPos<'a>) -> String {
    let mut s = if cur.consume2('r', '#') {
        String::from("r#")
    } else {
        String::new()
    };

    while is_id_body(cur.peekz()) {
        s.push(cur.peekz());
        cur.advance();
    }
    s
}

fn tokenize(text: &str) -> Result<Vec<Token>, String> {
    let mut tokens = vec![];
    let mut cur = CharPos::from_str(text);

    loop {
        skip_ws(&mut cur);
        let pos0 = cur.pos;
        if cur.peek().is_none() {
            break;
        }

        if is_id_start(cur.peekz()) {
            let id = read_keyword_or_identifier(&mut cur);
            let token_kind = match id.as_str() {
                "fn" => TokenKind::Fn,
                _ => TokenKind::Identifier(id),
            };
            tokens.push(Token::new(token_kind, pos0));
            continue;
        }

        let gram1 = match cur.peekz() {
            '(' => Some(TokenKind::LParen),
            ')' => Some(TokenKind::RParen),
            '{' => Some(TokenKind::LCurly),
            '}' => Some(TokenKind::RCurly),
            _ => None,
        };

        if let Some(kind) = gram1 {
            tokens.push(Token::new(kind, pos0));
            cur.advance();
            continue;
        }

        return Err(format!("Unknown token around {:?}", cur.pos));
    }

    return Ok(tokens);
}

struct Tokens<'a> {
    tokens: &'a [Token],
}

impl<'a> Tokens<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn kind_matches(&self, i: usize, kind: TokenKind) -> bool {
        self.tokens.get(i).map_or(false, |t| {
            std::mem::discriminant(&t.kind) == std::mem::discriminant(&kind)
        })
    }

    pub fn get_identifier(&self, i: usize) -> Option<String> {
        self.tokens.get(i).and_then(|t| {
            if let TokenKind::Identifier(name) = &t.kind {
                Some(name.clone())
            } else {
                None
            }
        })
    }
}

impl<'a> Index<usize> for Tokens<'a> {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

/// Concrete syntax tree: functions
#[derive(Debug)]
struct CSTFn {
    name: String,
    pos: Pos,
    body: Vec<CSTNode>,
}

impl CSTFn {
    pub fn new(name: String, pos: Pos) -> Self {
        Self {
            name,
            pos,
            body: vec![],
        }
    }
}

/// Concrete syntax tree nodes
#[derive(Debug)]
enum CSTNodeKind {
    Fn(CSTFn),
}

#[derive(Debug)]
struct CSTNode {
    kind: CSTNodeKind,
    pos: Pos,
}

impl CSTNode {
    fn new(kind: CSTNodeKind, pos: Pos) -> Self {
        Self { kind, pos }
    }
}

#[derive(Debug)]
struct CST {
    entries: HashMap<String, CSTNode>,
}

impl CST {
    pub fn new() -> Self {
        CST {
            entries: Default::default(),
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

    pub fn parse_fn(toks: &Tokens, index: &mut usize) -> Result<CSTFn, String> {
        let mut i = *index;
        let i0 = i;

        // FN
        if !toks.kind_matches(i, TokenKind::Fn) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err("function declaration: expectects fn".to_string());
        }
        i += 1;

        // <ident>
        let name = match toks.get_identifier(i) {
            Some(name) => name,
            None => {
                *index = Self::error_recovery_find_completed_block(toks, i);
                return Err("identifier(function name) expected".to_string());
            }
        };
        i += 1;

        // Args
        if !toks.kind_matches(i, TokenKind::LParen) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err("function declaration: arguments expected".to_string());
        }
        i += 1;

        // )
        if !toks.kind_matches(i, TokenKind::RParen) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err("function declaration: end of arguments expected".to_string());
        }
        i += 1;

        // {
        if !toks.kind_matches(i, TokenKind::LCurly) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err("function definition: code block expected".to_string());
        }
        i += 1;

        // }
        if !toks.kind_matches(i, TokenKind::RCurly) {
            *index = Self::error_recovery_find_completed_block(toks, i);
            return Err("function definition: end of block expected".to_string());
        }
        i += 1;

        *index = i;
        Ok(CSTFn::new(name, toks[i0].pos))
    }

    fn chkerr_check_name_duplicate(&self, name: &str) -> Option<String> {
        if let Some(other) = self.entries.get(name) {
            return Some(format!(
                "Duplicate of the name {}, previously the name was used at {:?}",
                name, other.pos
            ));
        } else {
            None
        }
    }

    pub fn from_tokens(tokens: &[Token]) -> Result<CST, String> {
        let toks = Tokens::new(tokens);
        let mut i = 0;
        let mut cst = CST::new();

        while i < toks.len() {
            let i0 = i;
            match toks[i].kind {
                TokenKind::Fn => {
                    let func = Self::parse_fn(&toks, &mut i)?;
                    if let Some(msg) = cst.chkerr_check_name_duplicate(&func.name) {
                        return Err(toks[i0].pos.report(msg));
                    }
                    cst.entries.insert(
                        func.name.clone(),
                        CSTNode::new(CSTNodeKind::Fn(func), toks[i0].pos),
                    );
                }
                _ => {
                    return Err(toks[i]
                        .pos
                        .report(format!("Unexpected top-level token {:?}", toks[i].kind)));
                }
            }
        }
        Ok(cst)
    }
}

fn main() {
    let source = " fn main() {} ";
    let tokens = tokenize(source).unwrap();
    let cst = CST::from_tokens(&tokens).unwrap();
    println!("{:?}", cst)
}

#[cfg(test)]
mod test_cst {
    use crate::{CST, Tokens, tokenize};

    #[test]
    fn test_err_rec1() {
        let t = tokenize("fn hello world(){}}").unwrap();
        let t = Tokens::new(&t);
        assert_eq!(CST::error_recovery_find_completed_block(&t, 2), 7);
        assert_eq!(CST::error_recovery_find_completed_block(&t, 5), 7);
        assert_eq!(CST::error_recovery_find_completed_block(&t, 6), 7);
    }
    #[test]
    fn test_err_rec2() {
        let t = tokenize("fn hello world()").unwrap();
        let t = Tokens::new(&t);
        assert_eq!(CST::error_recovery_find_completed_block(&t, 2), 5);
        assert_eq!(CST::error_recovery_find_completed_block(&t, 4), 5);
    }
    #[test]
    fn test_err_rec3() {
        let t = tokenize("fn hello world()").unwrap();
        let t = Tokens::new(&t);
        assert_eq!(CST::error_recovery_find_completed_block(&t, 100), 100);
    }
}

#[cfg(test)]
#[path = "_tests/test_lexer.rs"]
mod test_lexer;
