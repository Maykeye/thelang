use crate::{CST, Tokens, tokenize};

#[test]
fn test_err_rec1() {
    let t = tokenize("fn hello world(){}}").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 2), 7);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 5), 7);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 6), 7);
}
#[test]
fn test_err_rec2() {
    let t = tokenize("fn hello world()").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 2), 5);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 4), 5);
}
#[test]
fn test_err_rec3() {
    let t = tokenize("fn hello world()").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_completed_block(&t, 100), 100);
}
