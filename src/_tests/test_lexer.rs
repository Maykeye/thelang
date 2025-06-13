use crate::Token;
use crate::lexer::{read_keyword_or_identifier, skip_ws, tokenize};
use crate::tokens::Pos;
use crate::{CharPos, TokenKind};

#[test]
fn test_advance_on_empty() {
    let src = "";
    let mut char_pos = CharPos::from_str(src);
    assert!(char_pos.advance());
    assert_eq!(char_pos.pos, Pos::new(1, 1));
    assert_eq!(char_pos.offset, 0);
    // repeat advance over eof
    assert!(char_pos.advance());
    assert_eq!(char_pos.pos, Pos::new(1, 1));
    assert_eq!(char_pos.offset, 0);
}

#[test]
fn test_advance_base() {
    let src = "1\n2";
    let mut char_pos = CharPos::from_str(src);
    assert_eq!(char_pos.pos, Pos::new(1, 1));
    assert_eq!(char_pos.offset, 0);
    assert_eq!(char_pos.peekz(), '1');
    assert!(char_pos.peek().is_some());
    assert!(!char_pos.advance());
    assert_eq!(char_pos.pos, Pos::new(1, 2));
    assert_eq!(char_pos.offset, 1);
    assert_eq!(char_pos.peekz(), '\n');
    assert!(char_pos.peek().is_some());
    assert!(!char_pos.advance());
    assert_eq!(char_pos.pos, Pos::new(2, 1));
    assert_eq!(char_pos.offset, 2);
    assert_eq!(char_pos.peekz(), '2');
    assert!(char_pos.peek().is_some());
    assert!(char_pos.advance());
    assert_eq!(char_pos.pos, Pos::new(2, 2));
    assert_eq!(char_pos.offset, 3);
    assert!(char_pos.peek().is_none());
    assert_eq!(char_pos.peekz(), '\0');
    // repeat advance over EOF
    assert!(char_pos.advance());
    assert_eq!(char_pos.pos, Pos::new(2, 2));
    assert_eq!(char_pos.offset, 3);
    assert!(char_pos.peek().is_none());
    assert_eq!(char_pos.peekz(), '\0');
}

#[test]
fn test_skipws_none() {
    let mut cp_dense = CharPos::from_str("dense");
    skip_ws(&mut cp_dense);
    assert_eq!(cp_dense.pos, Pos::new(1, 1));
    assert_eq!(cp_dense.offset, 0);
    let mut cp_eof = CharPos::from_str("");
    skip_ws(&mut cp_eof);
    assert_eq!(cp_eof.pos, Pos::new(1, 1));
    assert_eq!(cp_eof.offset, 0);
}

#[test]
fn test_skipws_prefix() {
    let mut cp_data = CharPos::from_str("  data");
    skip_ws(&mut cp_data);
    assert_eq!(cp_data.pos, Pos::new(1, 3));
    assert_eq!(cp_data.offset, 2);
}

#[test]
fn test_singleline_comment() {
    let mut cp = CharPos::from_str("//3456789");
    skip_ws(&mut cp);
    assert_eq!(cp.pos, Pos::new(1, 10));
    assert_eq!(cp.offset, 9);
    let mut cp = CharPos::from_str("//");
    skip_ws(&mut cp);
    assert_eq!(cp.pos, Pos::new(1, 3));
    assert_eq!(cp.offset, 2);
}

#[test]
fn test_skipws_combo() {
    let mut cp = CharPos::from_str(" //1\n //2\n\n//\n d");
    skip_ws(&mut cp);
    assert_eq!(cp.pos, Pos::new(5, 2));
    assert_eq!(cp.offset, 15);
}

#[test]
fn test_skipws_vs_div() {
    let mut cp = CharPos::from_str(" / //");
    skip_ws(&mut cp);
    assert_eq!(cp.pos, Pos::new(1, 2));
    assert_eq!(cp.offset, 1);
}

#[test]
fn test_read_ident_smallest() {
    let mut cp = CharPos::from_str("A");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "A");
    assert_eq!(cp.pos, Pos::new(1, 2));
    assert_eq!(cp.offset, 1);
}

#[test]
fn test_read_ident() {
    let mut cp = CharPos::from_str("A12?");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "A12");
    assert_eq!(cp.pos, Pos::new(1, 4));
    assert_eq!(cp.offset, 3);
}

#[test]
fn test_read_ident_on_eol() {
    let mut cp = CharPos::from_str("Abc\n");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "Abc");
    assert_eq!(cp.pos, Pos::new(1, 4));
    assert_eq!(cp.offset, 3);
}

#[test]
fn test_read_ident_w_dollar() {
    let mut cp = CharPos::from_str("A$c\n");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "A$c");
    assert_eq!(cp.pos, Pos::new(1, 4));
    assert_eq!(cp.offset, 3);
}
#[test]
fn test_read_ident_w_underscore() {
    let mut cp = CharPos::from_str("A_c\n");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "A_c");
    assert_eq!(cp.pos, Pos::new(1, 4));
    assert_eq!(cp.offset, 3);
}

#[test]
fn test_read_ident_underscore() {
    let mut cp = CharPos::from_str("_\n");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "_");
    assert_eq!(cp.pos, Pos::new(1, 2));
    assert_eq!(cp.offset, 1);
}

#[test]
fn test_cjk_ident() {
    let mut cp = CharPos::from_str("A１２\n");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "A１２");
    assert_eq!(cp.pos, Pos::new(1, 4));
    assert_eq!(cp.offset, 3);
}

#[test]
fn test_raw_ident() {
    let mut cp = CharPos::from_str("r#123\n");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "r#123");
    assert_eq!(cp.pos, Pos::new(1, 6));
    assert_eq!(cp.offset, 5);
}

#[test]
fn test_empty_raw_ident() {
    let mut cp = CharPos::from_str("r#\n");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "r#");
    assert_eq!(cp.pos, Pos::new(1, 3));
    assert_eq!(cp.offset, 2);
}

#[test]
fn test_empty_non_raw_ident() {
    let mut cp = CharPos::from_str("myr#ident\n");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "myr");
    assert_eq!(cp.pos, Pos::new(1, 4));
    assert_eq!(cp.offset, 3);

    let mut cp = CharPos::from_str("r");
    let id = read_keyword_or_identifier(&mut cp);
    assert_eq!(id, "r");
    assert_eq!(cp.pos, Pos::new(1, 2));
    assert_eq!(cp.offset, 1);
}

#[test]
fn test_gram1() {
    let toks = tokenize("\n({});:,!-").unwrap();
    assert_eq!(toks[0], Token::new(TokenKind::LParen, Pos::new(2, 1)));
    assert_eq!(toks[1], Token::new(TokenKind::LCurly, Pos::new(2, 2)));
    assert_eq!(toks[2], Token::new(TokenKind::RCurly, Pos::new(2, 3)));
    assert_eq!(toks[3], Token::new(TokenKind::RParen, Pos::new(2, 4)));
    assert_eq!(toks[4], Token::new(TokenKind::Semi, Pos::new(2, 5)));
    assert_eq!(toks[5], Token::new(TokenKind::Colon, Pos::new(2, 6)));
    assert_eq!(toks[6], Token::new(TokenKind::Comma, Pos::new(2, 7)));
    assert_eq!(toks[7], Token::new(TokenKind::Exclamation, Pos::new(2, 8)));
    assert_eq!(toks[8], Token::new(TokenKind::Minus, Pos::new(2, 9)));
    assert_eq!(toks.len(), 9);
}

#[test]
fn test_gram2() {
    let toks = tokenize("\n->").unwrap();
    assert_eq!(toks[0], Token::new(TokenKind::ThinArrow, Pos::new(2, 1)));
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
            Token::new(TokenKind::Identifier("r".to_string()), Pos::new(1, 1)),
            Token::new(TokenKind::Identifier("r#".to_string()), Pos::new(1, 3)),
            Token::new(TokenKind::Identifier("r#aw".to_string()), Pos::new(1, 6)),
        ]
    );
}

#[test]
fn test_tokenize_keywords() {
    let toks: Vec<_> = tokenize("fn\n  return").unwrap().into_iter().collect();
    assert_eq!(&toks[0].kind, &TokenKind::Fn);
    assert_eq!(toks[0].pos, Pos::new(1, 1));
    assert_eq!(&toks[1].kind, &TokenKind::Return);
    assert_eq!(toks[1].pos, Pos::new(2, 3));
}
