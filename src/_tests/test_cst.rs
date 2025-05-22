use crate::cst::NodeKind;
use crate::tokens::Tokens;
use crate::{CST, tokenize};

fn cst_fom_text(text: &str) -> CST {
    let t = tokenize(text).unwrap();
    CST::from_tokens(&t).unwrap()
}
fn cst_err_fom_text(text: &str) -> (CST, Vec<String>) {
    let t = tokenize(text).unwrap();
    CST::from_tokens(&t).unwrap_err()
}
fn assert_only_err(text: &str, err_msg: &str) {
    let (cst, err) = cst_err_fom_text(text);
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
        let cst = cst_fom_text(text);
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
    let (cst, err) = cst_err_fom_text(
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
    let cst = cst_fom_text("fn fun(){{};{{}}}");
    let body = cst.functions.get("fun").unwrap();
    let body = body.body.as_ref().unwrap().as_ref();
    println!("{:?}", body);
    assert_eq!(body.nodes.len(), 2);
}
