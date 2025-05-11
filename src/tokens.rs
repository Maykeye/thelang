use std::{fmt::Display, iter::Peekable, ops::Index, str::Chars};

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
pub struct Tokens<'a> {
    tokens: &'a [Token],
}

impl<'a> Tokens<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn kind_eq(&self, i: usize, kind: TokenKind) -> bool {
        self.tokens.get(i).map_or(false, |t| t.kind == kind)
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
