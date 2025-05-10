use std::{collections::HashMap, fmt::Display, iter::Peekable, str::Chars};

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

#[derive(Debug)]
pub struct CompilationUnitTokens {
    pub functions: HashMap<String, Vec<Token>>,
}

fn try_get_token_identifier(token: &Token) -> Option<String> {
    if let TokenKind::Identifier(name) = &token.kind {
        Some(name.clone())
    } else {
        None
    }
}

impl CompilationUnitTokens {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
        }
    }

    /// This function goes through blocks that can be nested, eg (()) parenthesis and {{}} curly
    /// Startin block must be equal to `open`
    fn feed_forward_nested_blocks(
        tokens: &[Token],
        start: usize,
        open: TokenKind,
        close: TokenKind,
    ) -> Option<usize> {
        let mut i = start;
        let mut level = 0;

        // Check we are at the beginning of the (possibly nested) block
        if !tokens.get(i).map_or(false, |t| t.kind == open) {
            return None;
        }
        loop {
            if i >= tokens.len() {
                return None;
            }
            if tokens[i].kind == open {
                level += 1;
            } else if tokens[i].kind == close {
                level -= 1;
                if level == 0 {
                    i += 1;
                    break;
                }
            }
            i += 1;
        }
        Some(i)
    }

    pub fn from_tokens(tokens: Vec<Token>) -> Result<Self, String> {
        let mut i = 0;
        let mut cu = Self::new();

        let err = |p: usize, s: String| -> Result<Self, String> {
            return Err(tokens[p.min(tokens.len() - 1)].pos.report(format!("{}", s)));
        };

        while i < tokens.len() {
            if tokens[i].kind == TokenKind::Fn {
                if i + 1 >= tokens.len() {
                    return err(i, format!("function declaration: unexpected end of file"));
                }
                let name = match try_get_token_identifier(&tokens[i + 1]) {
                    Some(name) => name,
                    None => {
                        return err(i + 1, format!("function declaration: identifier expected"));
                    }
                };

                // Skip args
                let mut j = i + 2;
                let ok = tokens.get(j).map_or(false, |t| t.kind == TokenKind::LParen);
                if !ok {
                    return err(j, format!("function declaration: invalid argument parsing"));
                }

                let next_j = Self::feed_forward_nested_blocks(
                    &tokens,
                    j,
                    TokenKind::LParen,
                    TokenKind::RParen,
                );
                j = match next_j {
                    Some(j) => j,
                    None => {
                        return err(j, "unable to feed-forward args block".to_string());
                    }
                };

                let ok = tokens.get(j).map_or(false, |t| t.kind == TokenKind::LCurly);
                if !ok {
                    return err(j, format!("function declaration: LCurly expected"));
                }

                let next_j = Self::feed_forward_nested_blocks(
                    &tokens,
                    j,
                    TokenKind::LCurly,
                    TokenKind::RCurly,
                );
                j = match next_j {
                    Some(j) => j,
                    None => {
                        return err(j, "unable to feed-forward code block".to_string());
                    }
                };

                let vec = tokens[i..j].iter().map(|t| t.clone()).collect();

                if cu.functions.contains_key(&name) {
                    return Err(tokens[i]
                        .pos
                        .report(format!("function {} already was defined", name)));
                }
                cu.functions.insert(name, vec);
                i = j;
                continue;
            }
            return Err(format!("Invalid token around {:?}", tokens[i].pos));
        }

        Ok(cu)
    }
}

fn main() {
    let source = " fn main() {} ";
    let tokens = tokenize(source).unwrap();
    let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
    println!("{:?}", comp_unit_tokens)
}

#[cfg(test)]
fn toks_to_kinds(toks: &[Token]) -> Vec<TokenKind> {
    toks.iter().map(|x| x.kind.clone()).collect()
}

#[cfg(test)]
mod test_compilation_unit_tokens {
    use crate::{CompilationUnitTokens, TokenKind, tokenize, toks_to_kinds};

    #[test]
    fn test_happy_path() {
        let source = "fn main() {}\nfn foobar(){{}}";
        let tokens = tokenize(source).unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens).unwrap();
        let funcs = &comp_unit_tokens.functions;
        assert_eq!(funcs.len(), 2);
        let main = toks_to_kinds(&funcs["main"]);
        assert_eq!(
            main,
            [
                TokenKind::Fn,
                TokenKind::Identifier("main".to_string()),
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LCurly,
                TokenKind::RCurly,
            ]
        );

        let foobar = toks_to_kinds(&funcs["foobar"]);
        assert_eq!(
            foobar,
            [
                TokenKind::Fn,
                TokenKind::Identifier("foobar".to_string()),
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LCurly,
                TokenKind::LCurly,
                TokenKind::RCurly,
                TokenKind::RCurly,
            ]
        );
    }

    #[test]
    fn test_arg_error() {
        let tokens = tokenize("fn main((){}").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());

        let tokens = tokenize("fn main()){}").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());

        let tokens = tokenize("fn main{}").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());

        let tokens = tokenize("fn main(").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());
    }

    #[test]
    fn test_body_error() {
        let tokens = tokenize("fn main(){{}").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());

        let tokens = tokenize("fn main(){}}").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());

        let tokens = tokenize("fn main()").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());

        let tokens = tokenize("fn main(){").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());
    }

    #[test]
    fn test_duplicate_names() {
        let tokens = tokenize("fn main(){}\nfn okk(){}\nfn main(){}").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());
    }

    #[test]
    fn test_invalid_top_level() {
        let tokens = tokenize("Sing the strongest song").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());
    }

    #[test]
    fn test_sudden_eof_in_fn() {
        let tokens = tokenize("fn main").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);

        assert!(comp_unit_tokens.is_err());
        let tokens = tokenize("fn").unwrap();
        let comp_unit_tokens = CompilationUnitTokens::from_tokens(tokens);
        assert!(comp_unit_tokens.is_err());
    }
}

#[cfg(test)]
#[path = "_tests/test_lexer.rs"]
mod test_lexer;
