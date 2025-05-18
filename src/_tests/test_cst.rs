use crate::cst::NodeKind;
use crate::tokens::Tokens;
use crate::{CST, tokenize};

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
fn test_several_errors_recovery() {
    let t =
        tokenize("fn hello world(){} fn really hello(){} fn fine(){} fn aaa {}  fn fine_too(){} ")
            .unwrap();
    let cst = CST::from_tokens(&t);
    let (cst, err) = cst.unwrap_err();
    assert_eq!(cst.functions.len(), 2);
    assert!(cst.functions.contains_key("fine"));
    assert!(cst.functions.contains_key("fine_too"));
    assert_eq!(err.len(), 3);
}

#[test]
fn test_ret() {
    fn r#impl(text: &str) {
        println!("{text}");
        let t = tokenize(text).unwrap();
        let cst = CST::from_tokens(&t).unwrap();
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
