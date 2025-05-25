use crate::cst::NodeKind;
use crate::tokens::Tokens;
use crate::{CST, tokenize};

fn cst_from_text(text: &str) -> CST {
    let t = tokenize(text).expect(&format!("Lexer failed for {text}"));
    CST::from_tokens(&t).expect(&format!("CST failed for {text}"))
}
fn cst_err_from_text(text: &str) -> (CST, Vec<String>) {
    let t = tokenize(text).unwrap();
    CST::from_tokens(&t).unwrap_err()
}
fn assert_only_err(text: &str, err_msg: &str) {
    let (cst, err) = cst_err_from_text(text);
    assert_eq!(err.len(), 1);
    assert!(cst.functions.is_empty());
    assert_eq!(err[0], err_msg);
}

#[test]
fn test_err_rec_extra_rcurly() {
    let t = tokenize("fn hello world(){}}").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 2), 7);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 5), 7);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 6), 7);
}
#[test]
fn test_err_rec_no_body() {
    let t = tokenize("fn hello world()").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 2), 5);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 4), 5);
}

#[test]
fn test_err_rec_idx() {
    let t = tokenize("fn hello world()").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 100), 100);
}

#[test]
fn test_ret() {
    fn r#impl(text: &str) {
        println!("{text}");
        let cst = cst_from_text(text);
        let func = cst.functions.get("hello").unwrap();
        let body = func.body.as_ref().unwrap();
        let node0 = &body.nodes[0].kind;

        match node0 {
            NodeKind::Return(node) => {
                assert!(matches!(node.kind, NodeKind::Unit));
            }
            _ => panic!("return expected"),
        };
    }
    r#impl("fn hello(){return;}");
    r#impl("fn hello(){return}");
    r#impl("fn hello(){return ();}");
    r#impl("fn hello(){return ()}");
}

#[test]
fn test_func_name_err_recovery() {
    let (cst, err) = cst_err_from_text(
        "fn hello world(){} fn really hello(){} fn fine(){} fn aaa {}  fn fine_too(){} ",
    );

    assert_eq!(cst.functions.len(), 2);
    assert!(cst.functions.contains_key("fine"));
    assert!(cst.functions.contains_key("fine_too"));
    assert_eq!(err.len(), 3);
}

#[test]
fn test_func_block_err_recovery() {
    assert_only_err(
        "fn err(){)}",
        "1: 10: function definition: unsupported token: RParen",
    );

    assert_only_err(
        "fn err(){{)}}",
        "1: 11: function definition: unsupported token: RParen",
    );
}

#[test]
fn test_nested_blocks() {
    let cst = cst_from_text("fn fun(){{};{{}}}");
    let body = cst.functions.get("fun").unwrap();
    let body = body.body.as_ref().unwrap().as_ref();
    assert_eq!(body.nodes.len(), 2);
    match &body.nodes[0].kind {
        NodeKind::CodeBlock(nested) => {
            assert_eq!(nested.nodes.len(), 0);
        }
        _ => panic!(">> unexpected node kind {:?}", &body.nodes[0]),
    }
    match &body.nodes[1].kind {
        NodeKind::CodeBlock(nested) => {
            assert_eq!(nested.nodes.len(), 1);
            match &nested.nodes[0].kind {
                NodeKind::CodeBlock(nested) => {
                    assert_eq!(nested.nodes.len(), 0);
                }
                _ => panic!(">> unexpected node kind {:?}", &body.nodes[0]),
            };
        }
        _ => panic!(">> unexpected node kind {:?}", &body.nodes[0]),
    }
}

#[test]
fn test_arg_count() {
    fn r#impl(source: &str, n: usize) {
        let cst = cst_from_text(source);
        assert_eq!(cst.functions["fun"].args.len(), n, "source: {source}");
    }
    r#impl("fn fun(){}", 0);
    r#impl("fn fun(a:a){}", 1);
    r#impl("fn fun(a:a,){}", 1);
    r#impl("fn fun(a:(), a:()){}", 2);
    r#impl("fn fun(a:a, a:a,){}", 2);
    r#impl("fn fun(a:a, a:(), a:()){}", 3);
    r#impl("fn fun(a:a, a:(), a:(),){}", 3);
}
#[test]
fn test_err_with_args() {
    cst_err_from_text("fn fun(,){}");
    cst_err_from_text("fn fun(a:){}");
    cst_err_from_text("fn fun(a:,a:a){}");
    cst_err_from_text("fn fun(:a){}");
    cst_err_from_text("fn fun(a:a,a:){}");
    cst_err_from_text("fn fun(a:a,:a){}");
    cst_err_from_text("fn fun(a:a,,){}");
}

#[test]
fn test_arg_names() {
    fn r#impl(source: &str, args: &[(&str, &str)]) {
        let cst = cst_from_text(source);
        assert_eq!(
            cst.functions["fun"].args.len(),
            args.len(),
            "source: {source}"
        );
        for i in 0..args.len() {
            let arg = &cst.functions["fun"].args[i];
            assert_eq!(
                arg.name, args[i].0,
                "arg#{i} name mismatch; source: {source}"
            );
            if let NodeKind::Identifier(r#type) = &arg.r#type.kind {
                assert_eq!(r#type, args[i].1, "arg#{i} name mismatch; source: {source}")
            } else {
                let ok = matches!(arg.r#type.kind, NodeKind::Unit) && args[i].1 == "()";
                assert!(ok, "invalid arg#{i} type {:?}: {source}", arg.r#type.kind);
            }
        }
    }

    r#impl("fn fun(){}", &[]);
    r#impl("fn fun(a:b){}", &[("a", "b")]);
    r#impl("fn fun(a:_){}", &[("a", "_")]);
    r#impl("fn fun(a1:_, a2:_){}", &[("a1", "_"), ("a2", "_")]);
    r#impl("fn fun(_:(), foo: bar,){}", &[("_", "()"), ("foo", "bar")]);
}
