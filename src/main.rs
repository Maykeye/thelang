use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq, PartialOrd)]
enum Token {
    Fn,
    Identifier(String),
    LParen,
    RParen,
    LCurly,
    RCurly,
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
        if cur.peek().is_none() {
            break;
        }

        if is_id_start(cur.peekz()) {
            let id = read_keyword_or_identifier(&mut cur);
            let token = match id.as_str() {
                "fn" => Token::Fn,
                _ => Token::Identifier(id),
            };
            tokens.push(token);
            continue;
        }

        let gram1 = match cur.peekz() {
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '{' => Some(Token::LCurly),
            '}' => Some(Token::RCurly),
            _ => None,
        };

        if let Some(token) = gram1 {
            tokens.push(token);
            cur.advance();
            continue;
        }

        return Err(format!("Unknown token around {:?}", cur.pos));
    }

    return Ok(tokens);
}

fn main() {
    let source = " fn main() {} ";
    let tokens = tokenize(source).unwrap();
    println!("{:?}", tokens)
}

#[cfg(test)]
mod test_lexer {
    use crate::{CharPos, Pos, Token, read_keyword_or_identifier, skip_ws, tokenize};

    #[test]
    fn test_advance_on_empty() {
        let src = "";
        let mut char_pos = CharPos::from_str(src);
        assert!(char_pos.advance());
        assert_eq!(char_pos.pos, Pos::new(1, 1, 0));
        // repeat advance over eof
        assert!(char_pos.advance());
        assert_eq!(char_pos.pos, Pos::new(1, 1, 0));
    }

    #[test]
    fn test_advance_base() {
        let src = "1\n2";
        let mut char_pos = CharPos::from_str(src);
        assert_eq!(char_pos.pos, Pos::new(1, 1, 0));
        assert_eq!(char_pos.peekz(), '1');
        assert!(char_pos.peek().is_some());
        assert!(!char_pos.advance());
        assert_eq!(char_pos.pos, Pos::new(1, 2, 1));
        assert_eq!(char_pos.peekz(), '\n');
        assert!(char_pos.peek().is_some());
        assert!(!char_pos.advance());
        assert_eq!(char_pos.pos, Pos::new(2, 1, 2));
        assert_eq!(char_pos.peekz(), '2');
        assert!(char_pos.peek().is_some());
        assert!(char_pos.advance());
        assert_eq!(char_pos.pos, Pos::new(2, 2, 3));
        assert!(char_pos.peek().is_none());
        assert_eq!(char_pos.peekz(), '\0');
        // repeat advance over EOF
        assert!(char_pos.advance());
        assert_eq!(char_pos.pos, Pos::new(2, 2, 3));
        assert!(char_pos.peek().is_none());
        assert_eq!(char_pos.peekz(), '\0');
    }

    #[test]
    fn test_skipws_none() {
        let mut cp_dense = CharPos::from_str("dense");
        skip_ws(&mut cp_dense);
        assert_eq!(cp_dense.pos, Pos::new(1, 1, 0));
        let mut cp_eof = CharPos::from_str("");
        skip_ws(&mut cp_eof);
        assert_eq!(cp_eof.pos, Pos::new(1, 1, 0));
    }

    #[test]
    fn test_skipws_prefix() {
        let mut cp_data = CharPos::from_str("  data");
        skip_ws(&mut cp_data);
        assert_eq!(cp_data.pos, Pos::new(1, 3, 2));
    }

    #[test]
    fn test_singleline_comment() {
        let mut cp = CharPos::from_str("//3456789");
        skip_ws(&mut cp);
        assert_eq!(cp.pos, Pos::new(1, 10, 9));
        let mut cp = CharPos::from_str("//");
        skip_ws(&mut cp);
        assert_eq!(cp.pos, Pos::new(1, 3, 2));
    }

    #[test]
    fn test_skipws_combo() {
        let mut cp = CharPos::from_str(" //1\n //2\n\n//\n d");
        skip_ws(&mut cp);
        assert_eq!(cp.pos, Pos::new(5, 2, 15));
    }

    #[test]
    fn test_skipws_vs_div() {
        let mut cp = CharPos::from_str(" / //");
        skip_ws(&mut cp);
        assert_eq!(cp.pos, Pos::new(1, 2, 1));
    }

    #[test]
    fn test_read_ident_smallest() {
        let mut cp = CharPos::from_str("A");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "A");
        assert_eq!(cp.pos, Pos::new(1, 2, 1));
    }

    #[test]
    fn test_read_ident() {
        let mut cp = CharPos::from_str("A12?");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "A12");
        assert_eq!(cp.pos, Pos::new(1, 4, 3));
    }

    #[test]
    fn test_read_ident_on_eol() {
        let mut cp = CharPos::from_str("Abc\n");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "Abc");
        assert_eq!(cp.pos, Pos::new(1, 4, 3));
    }

    #[test]
    fn test_read_ident_w_dollar() {
        let mut cp = CharPos::from_str("A$$es\n");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "A$$es");
        assert_eq!(cp.pos, Pos::new(1, 6, 5));
    }

    #[test]
    fn test_cjk_ident() {
        let mut cp = CharPos::from_str("A１２\n");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "A１２");
        assert_eq!(cp.pos, Pos::new(1, 4, 3));
    }

    #[test]
    fn test_raw_ident() {
        let mut cp = CharPos::from_str("r#123\n");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "r#123");
        assert_eq!(cp.pos, Pos::new(1, 6, 5));
    }

    #[test]
    fn test_empty_raw_ident() {
        let mut cp = CharPos::from_str("r#\n");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "r#");
        assert_eq!(cp.pos, Pos::new(1, 3, 2));
    }

    #[test]
    fn test_empty_non_raw_ident() {
        let mut cp = CharPos::from_str("myr#ident\n");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "myr");
        assert_eq!(cp.pos, Pos::new(1, 4, 3));

        let mut cp = CharPos::from_str("r");
        let id = read_keyword_or_identifier(&mut cp);
        assert_eq!(id, "r");
        assert_eq!(cp.pos, Pos::new(1, 2, 1));
    }

    #[test]
    fn test_tokenize() {
        let toks = tokenize("fn ({identifier})").unwrap();
        assert_eq!(
            toks,
            [
                Token::Fn,
                Token::LParen,
                Token::LCurly,
                Token::Identifier("identifier".to_string()),
                Token::RCurly,
                Token::RParen,
            ]
        );
    }

    #[test]
    fn test_tokenize_err() {
        let toks = tokenize("▞");
        assert!(toks.is_err());
    }

    #[test]
    fn test_tokenize_empty() {
        let toks = tokenize("");
        assert_eq!(toks.unwrap(), []);
        let toks = tokenize("//comment only");
        assert_eq!(toks.unwrap(), []);
        let toks = tokenize("//comments\n//only\n");
        assert_eq!(toks.unwrap(), []);
        let toks = tokenize(" ");
        assert_eq!(toks.unwrap(), []);
        let toks = tokenize("\n");
        assert_eq!(toks.unwrap(), []);
    }

    #[test]
    fn test_tokenizer_over_raw_identifier() {
        let toks = tokenize("r r# r#aw");
        assert_eq!(
            toks.unwrap(),
            [
                Token::Identifier("r".to_string()),
                Token::Identifier("r#".to_string()),
                Token::Identifier("r#aw".to_string()),
            ]
        );
    }

    //
}
