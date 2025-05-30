// This file parses overall grammar without going too deep into nested tokens
use crate::cst::NodeKind;
use crate::tokens::Pos;
use crate::{CST, tokenize};
fn cst_fun_from_text(text: &str, func_name: &str) -> crate::cst::Fn {
    let t = tokenize(text).expect(&format!("Lexer failed for {text}"));
    let mut cst = CST::from_tokens(&t).expect(&format!("CST failed for <<<\n{text}\n>>>"));
    cst.functions.remove(func_name).expect(&format!(
        "can't extract function {func_name} for <<<\n{text}\n>>>"
    ))
}

fn test_expr<F: Fn(&NodeKind)>(expr_text: &str, delta_col: usize, f: F) {
    const PURE_EXPR: usize = 0;
    const SFX_TAIL_SEMI: usize = 1;
    const SFX_TAIL_IDENT: usize = 2;

    let step = |tail_kind: usize| {
        // Build suffix:
        let sfx = match tail_kind {
            // Either nothing is attached to the <expr>
            PURE_EXPR => "",
            // Or it becomes <expr>;
            SFX_TAIL_SEMI => ";",
            // Or new expression is attached to check it's not being consumed too
            SFX_TAIL_IDENT => ";TAIL",
            _ => unreachable!("test internal error"),
        };
        let source = format!("fn foo(){{\n  {expr_text}\n{sfx}}}");
        let fun = cst_fun_from_text(&source, "foo");
        let body = fun
            .body
            .expect(&format!("function body not found for <<<\n{source}\n>>>"));
        assert_eq!(
            body.nodes.len(),
            if tail_kind == PURE_EXPR { 1 } else { 2 },
            "<<<\n{source}\n>>>\n{:?}",
            body.nodes
        );
        let node = &body.nodes[0];
        assert_eq!(
            node.pos,
            Pos::new(2, 3 + delta_col),
            "<<<\n{source}\n>>>\n{:?}",
            body.nodes
        );
        f(&node.kind);
        if tail_kind == SFX_TAIL_IDENT {
            let tail = body.nodes.last().unwrap();
            assert_eq!(
                tail.pos,
                Pos::new(3, 2),
                "<<<\n{source}\n>>>\n{:?}",
                body.nodes
            );
            assert!(matches!(
            &tail.kind, NodeKind::Identifier(x) if x == "TAIL"
            ));
        }
    };
    step(PURE_EXPR);
    step(SFX_TAIL_SEMI);
    step(SFX_TAIL_IDENT);
}

//---- TERM
#[test]
fn test_expr_term_unit() {
    test_expr("()", 0, |node| {
        assert!(matches!(&node, NodeKind::Unit));
    });
}
#[test]
fn test_expr_term_codeblock() {
    test_expr("{(a)}", 0, |node| {
        assert!(matches!(&node, NodeKind::CodeBlock(_)))
    });
}

#[test]
fn test_expr_term_identifier() {
    test_expr("var_name", 0, |node| {
        assert!(matches!(&node, NodeKind::Identifier(x) if x == "var_name"));
    });
}

#[test]
fn test_expr_term_parens() {
    test_expr("(abc)", 1, |node| {
        assert!(matches!(
            &node,
            NodeKind::Identifier(x) if x == "abc"
        ))
    });
}

//---- UNARY
#[test]
fn test_expr_unary_return_w_expr() {
    test_expr("return ret_val", 0, |node| match node {
        NodeKind::Return(rv) => {
            assert!(matches!(
                &rv.kind,
                NodeKind::Identifier(x) if x == "ret_val"
            ))
        }
        _ => panic!("return expected"),
    });
}
