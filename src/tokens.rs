use std::{fmt::Display, iter::Peekable, ops::Index, str::Chars};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /// Function keyword
    Fn,
    /// Identifier of anything
    Identifier(String),
    /// Binary-and or address-of
    Ampersand,
    /// Parenthisis
    LParen,
    RParen,
    /// Code block start
    LCurly,
    /// Code block ends
    RCurly,
    /// Minus
    Minus,
    /// Thin arrows (->) are used for return type
    ThinArrow,
    /// Invert of boolean
    Exclamation,
    /// Token separator
    Semi,
    /// Separates variable(or argument) name from type
    Colon,
    /// Variable separator
    Comma,
    /// Return keyword. Grammar is
    /// return <expr> ;
    /// return ;
    /// return <expr> }
    /// return }
    Return,

    /// END OF  FILE
    EOF,
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
    pub tokens: &'a [Token],
}

impl<'a> Tokens<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn kind_eq(&self, i: usize, kind: TokenKind) -> bool {
        //self.tokens.get(i).map_or(false, |t| t.kind == kind)
        self.tokens.get(i).is_some_and(|t| t.kind == kind)
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

    pub fn get_nth_kind_description(&self, i: usize) -> String {
        if i >= self.tokens.len() {
            "(EOF)".to_string()
        } else {
            format!("{:?}", self.tokens[i].kind)
        }
    }

    pub fn get_nth_kind(&self, i: usize) -> TokenKind {
        self.tokens
            .get(i)
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::EOF)
    }
    pub fn get_nth_pos(&self, i: usize) -> Pos {
        if self.len() == 0 {
            Pos::new(0, 0)
        } else {
            self.tokens[i.min(self.len() - 1)].pos
        }
    }

    pub fn inner(&self) -> &'a [Token] {
        self.tokens
    }
}

impl Index<usize> for Tokens<'_> {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Pos { line, col }
    }

    pub fn report<D: Display>(&self, msg: D) -> String {
        format!("{}:{}:{}", self.line, self.col, msg)
    }
}
impl Default for Pos {
    fn default() -> Self {
        Pos::new(1, 1)
    }
}
impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct CharPos<'a> {
    pub pos: Pos,
    pub it: Peekable<Chars<'a>>,
    pub offset: usize,
}
impl<'a> CharPos<'a> {
    pub fn from_str(txt: &'a str) -> Self {
        Self {
            pos: Pos::default(),
            it: txt.chars().peekable(),
            offset: 0,
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.it.peek().copied()
    }

    pub fn peekz(&mut self) -> char {
        self.peek().unwrap_or('\0')
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
                self.offset += 1;
            }
            _ => {
                self.pos.col += 1;
                self.offset += 1;
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
