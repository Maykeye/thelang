/// Skip whitespace(//comment line included into whitespace)
use crate::CharPos;
use crate::tokens::Pos;
use crate::{Token, TokenKind};
fn skip_ws(cur: &mut CharPos) {
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
fn read_keyword_or_identifier(cur: &mut CharPos) -> String {
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

#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    UnknownToken { pos: Pos, ch: char },
}

pub fn tokenize(text: &str) -> Result<Vec<Token>, LexerError> {
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
                "return" => TokenKind::Return,
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
            ';' => Some(TokenKind::Semi),
            ':' => Some(TokenKind::Colon),
            ',' => Some(TokenKind::Comma),
            '!' => Some(TokenKind::Exclamation),
            '&' => Some(TokenKind::Ampersand),
            _ => None,
        };

        if let Some(kind) = gram1 {
            tokens.push(Token::new(kind, pos0));
            cur.advance();
            continue;
        }

        let gram12 = match cur.peekz() {
            '-' => {
                cur.advance();
                if cur.peekz() == '>' {
                    cur.advance();
                    Some(TokenKind::ThinArrow)
                } else {
                    Some(TokenKind::Minus)
                }
            }
            _ => None,
        };
        if let Some(kind) = gram12 {
            tokens.push(Token::new(kind, pos0));
            continue;
        }

        // TODO: include better token info
        return Err(LexerError::UnknownToken {
            pos: cur.pos,
            ch: cur.peekz(),
        });
    }

    Ok(tokens)
}

#[cfg(test)]
#[path = "_tests/test_lexer.rs"]
mod test_lexer;
