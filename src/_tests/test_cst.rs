use crate::cst::NodeKind;
use crate::tokens::Tokens;
use crate::unwrap_variant;
use crate::{CST, tokenize};
use pretty_assertions::assert_eq;

fn cst_fun_from_text(text: &str, func_name: &str) -> crate::cst::Fn {
    let t = tokenize(text).expect(&format!("Lexer failed for {text}"));
    let mut cst = CST::from_tokens(&t).expect(&format!("CST failed for <<<\n{text}\n>>>"));
    cst.functions.remove(func_name).expect(&format!(
        "can't extract function {func_name} for <<<\n{text}\n>>>"
    ))
}
fn cst_err_from_text(text: &str) -> (CST, Vec<String>) {
    let t = tokenize(text).unwrap();
    CST::from_tokens(&t).unwrap_err()
}
fn assert_only_err(text: &str, err_msg: &str) {
    let (cst, err) = cst_err_from_text(text);
    assert_eq!(
        err.len(),
        1,
        "only one error expected, got: {err:?}\n{text}"
    );
    assert!(cst.functions.is_empty());
    assert_eq!(
        err[0], err_msg,
        "Source:\n{text}\n(Left = got; right = expected)"
    );
}

#[test]
fn test_err_rec_extra_rcurly() {
    let t = tokenize("fn hello world(){}}").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_next_block_end(&t, 2), 6);
    assert_eq!(CST::error_recovery_find_next_block_end(&t, 5), 6);
    assert_eq!(CST::error_recovery_find_next_block_end(&t, 6), 6);
}
#[test]
fn test_err_rec_no_body() {
    let t = tokenize("fn hello world()").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_next_block_end(&t, 2), 5);
    assert_eq!(CST::error_recovery_find_next_block_end(&t, 4), 5);
}

#[test]
fn test_err_rec_idx() {
    let t = tokenize("fn hello world()").unwrap();
    let t = Tokens::new(&t);
    assert_eq!(CST::error_recovery_find_next_block_end(&t, 100), 100);
}

#[test]
fn test_ret() {
    fn r#impl(text: &str) {
        let func = cst_fun_from_text(text, "hello");
        let body = func.body.as_ref().unwrap();
        let node0 = &body.nodes[0].kind;
        let returned_node = unwrap_variant!(node0, NodeKind::Return);
        unwrap_variant!(returned_node.kind, NodeKind::Unit, ());
    }
    r#impl("fn hello(){return;}");
    r#impl("fn hello(){return}");
    r#impl("fn hello(){return ();}");
    r#impl("fn hello(){return ()}");
}

#[test]
fn test_return_type_unit_type() {
    let func = cst_fun_from_text("fn func_name() -> (){}", "func_name");
    let ret_type = func.return_type.unwrap();
    unwrap_variant!(ret_type.kind, NodeKind::Unit, ());
}
#[test]
fn test_return_type_named() {
    fn check(name: &str) {
        let func = cst_fun_from_text(&format!("fn func_name() -> {name}{{}}"), "func_name");
        let ret_type = func.return_type.unwrap();
        let x = unwrap_variant!(ret_type.kind, NodeKind::Identifier);
        assert_eq!(x, name);
    }

    check("bool");
    check("not_standard_type");
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
        "1: 10: expression required, found instead `RParen`",
    );

    assert_only_err(
        "fn err(){{)}}",
        "1: 11: expression required, found instead `RParen`",
    );
}

#[test]
fn test_nested_blocks() {
    let body = cst_fun_from_text("fn fun(){{};{{}}}", "fun");
    let body = body.body.as_ref().unwrap().as_ref();
    assert_eq!(body.nodes.len(), 2);
    let lhs_cb = unwrap_variant!(&body.nodes[0].kind, NodeKind::CodeBlock);
    assert!(lhs_cb.nodes.is_empty());

    let rhs_cb = unwrap_variant!(&body.nodes[1].kind, NodeKind::CodeBlock);
    assert_eq!(rhs_cb.nodes.len(), 1);
    let rhs_inner_cb = unwrap_variant!(&rhs_cb.nodes[0].kind, NodeKind::CodeBlock);
    assert!(rhs_inner_cb.nodes.is_empty());
}

#[test]
fn test_arg_count() {
    fn r#impl(source: &str, n: usize) {
        let func = cst_fun_from_text(source, "fun");
        assert_eq!(func.args.len(), n, "source: {source}");
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
        let fun = cst_fun_from_text(source, "fun");
        assert_eq!(fun.args.len(), args.len(), "source: {source}");
        for i in 0..args.len() {
            let arg = &fun.args[i];
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
