use crate::AST;
use crate::ast::{ExprKind, Type};
use crate::tokens::Pos;
use crate::{cst::CST, lexer::tokenize};

fn make_empty_func_ast() -> AST {
    let toks = tokenize("\nfn empty_func()\n{\n}").unwrap();
    let cst = CST::from_tokens(&toks).unwrap();
    let ast = AST::from_cst(cst).unwrap();
    ast
}

#[test]
fn test_empty_func_decl() {
    let empty_func = &make_empty_func_ast().functions["empty_func"];
    assert_eq!(empty_func.name, "empty_func");
    assert_eq!(empty_func.r#type.return_type, Type::Unit);
    assert!(empty_func.r#type.args.is_empty());
}

#[test]
fn test_empty_func_decl_pos() {
    let empty_func = &make_empty_func_ast().functions["empty_func"];
    assert_eq!(empty_func.decl_pos, Pos::new(2, 1));
    let body = empty_func.body.as_ref().unwrap();
    assert_eq!(body.pos.line, 3);
    assert_eq!(body.pos.col, 1);
}

#[test]
fn test_empty_func_definition() {
    let empty_func = &make_empty_func_ast().functions["empty_func"];
    let body = empty_func.body.as_ref().unwrap();
    assert_eq!(body.return_type, Some(Type::Unit));
    assert_eq!(body.exprs.len(), 1);
    let ret_expr = match &body.exprs[0].kind {
        ExprKind::Return(ret_val) => ret_val.as_ref(),
        _ => {
            panic!("expected Return, not {:?}", body.exprs[0].kind)
        }
    };
    assert!(ret_expr.is_none());
}

#[test]
fn test_nesting_return_type() {
    fn r#impl(txt: &str) {
        println!("testing {txt}");
        let toks = tokenize(txt).unwrap();
        let cst = CST::from_tokens(&toks).unwrap();
        let ast = AST::from_cst(cst).unwrap();
        let body = ast.functions.get("nesting").unwrap().body.as_ref().unwrap();
        assert_eq!(body.return_type, Some(Type::Unit));
    }

    r#impl("fn nesting()\n{{}}");
    r#impl("fn nesting()\n{{()}}");
    r#impl("fn nesting()\n{{()};}");
    r#impl("fn nesting()\n{{return ();};}");
    r#impl("fn nesting()\n{{return ();}}");
    r#impl("fn nesting()\n{{return ()};}");
    r#impl("fn nesting()\n{{return};}");
    r#impl("fn nesting()\n{{return}}");
    r#impl("fn nesting()\n{{{}}}");
    r#impl("fn nesting()\n{{};}");
    r#impl("fn nesting()\n{}");
}
