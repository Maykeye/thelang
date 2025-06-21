use crate::{
    IR,
    ir::{IRCodeBlockId, IROp, IRRegId, IRTypeId},
};

#[test]
fn test_ir() {
    //                        0         1         2         3          4
    //                        012345678901234567890123456789001234567890112
    let ir = IR::from_thelan("//example of an empty function\nfn main(){\n}");
    let ir = ir.unwrap();

    assert_eq!(ir.functions.len(), 1);
    let main = ir.functions.get("main").unwrap();
    assert_eq!(main.pos.line, 2);
    assert_eq!(main.pos.col, 1);
    assert_eq!(main.name, "main");
    assert_eq!(main.blocks.len(), 1);
    let blk = &main.blocks[0];
    assert_eq!(blk.id, IRCodeBlockId(0));
    assert_eq!(blk.ops.len(), 1);
    if let &IROp::Return { value: r } = &blk.ops[0] {
        assert_eq!(r, IRRegId(0));
    } else {
        panic!("ret expected");
    }
}

#[test]
fn test_arg_indices() {
    let source = " fn main(a0: (), a1:(), a2:()) {} ";
    let ir = IR::from_thelan(source).unwrap();
    let func = ir.functions.get("main").unwrap();
    assert_eq!(func.args.len(), 3);
    assert_eq!(func.get_reg_data(func.args[0]).argument_index, Some(0));
    assert_eq!(func.get_reg_data(func.args[1]).argument_index, Some(1));
    assert_eq!(func.get_reg_data(func.args[2]).argument_index, Some(2));
}

#[test]
fn test_arg_types() {
    let source = " fn main(none: ()) {} ";
    let ir = IR::from_thelan(source).unwrap();
    let func = ir.functions.get("main").unwrap();
    assert_eq!(func.args.len(), 1);
    assert_eq!(func.get_reg_data(func.args[0]).r#type, IRTypeId::UNIT);
    assert!(func.args[0].0 >= IRRegId::BUILTIN_REGS_COUNT);
}

#[test]
fn test_nested_implicit_return() {
    let source = " fn main() { {()} } ";
    let ir = IR::from_thelan(source).unwrap();
    let expected = "\
FUNC main
.b0:
$r1 = call .b1
ret $r1

.b1:
ret $r0:<()>

END FUNC main\n";
    assert_eq!(ir.to_text(), expected);
}

#[test]
fn test_return_arg() {
    fn r#impl(source: &str, return_expr: usize, reg_name: &str) {
        assert!(return_expr > 0);
        let ir = IR::from_thelan(source).expect(&format!("unable to get ast from {source}"));
        let func = ir.functions.get("func").unwrap();
        let block = func.blocks.get(0).unwrap();
        match block.ops[return_expr] {
            IROp::Return { value: ret_arg } => {
                let reg_ret_data = func.get_reg_data(ret_arg);
                assert!(
                    reg_ret_data.name.is_none(),
                    "Expected to load argument into nameless argument {:?} to return it\n{source}\n{:?}",
                    reg_ret_data.name,
                    block.ops
                );
                let (arg, dest) = match block.ops[return_expr - 1] {
                    IROp::LoadArg { arg, dest } => (arg, dest),
                    _ => panic!(
                        "Expected to load argument to return it\n{source}\n{:?}",
                        block.ops
                    ),
                };
                assert_eq!(
                    ret_arg, dest,
                    "Expected to load argument to return it\n{source}\n{:?}",
                    block.ops
                );
                let reg_arg_data = func.get_reg_data(arg);

                assert_eq!(
                    reg_arg_data.name,
                    Some(reg_name.to_string()),
                    "Register with a name {reg_name} expected, got {reg_arg_data:?}"
                )
            }
            _ => panic!(
                "return expected, not {:?}\n {source}\n{:?}",
                block.ops[return_expr], block.ops
            ),
        }
    }

    r#impl("fn func(a1: (), a2:()){return a1}", 1, "a1");
    r#impl("fn func(a1: (), a2:()){return a2}", 1, "a2");
    r#impl("fn func(a1: (), a2:()){return a1;}", 1, "a1");
    r#impl("fn func(a1: (), a2:()){return a2;}", 1, "a2");
    r#impl("fn func(a1: (), a2:()){a1}", 1, "a1");
    r#impl("fn func(a1: (), a2:()){a2}", 1, "a2");
    r#impl("fn func(a1: (), a2:()){a1;a2}", 2, "a2");
    r#impl("fn func(a1: (), a2:()){a2;a1}", 2, "a1");
    r#impl("fn func(a1: (), a2:()){return a1;a2}", 1, "a1");
    r#impl("fn func(a1: (), a2:()){return a2;a1;}", 1, "a2");
    r#impl("fn func(a1: (), a2:(), a3:()){a2;a1;a3}", 3, "a3");
}

#[test]
fn test_return_r0() {
    fn r#impl(source: &str) {
        let ir = IR::from_thelan(source).expect(&format!("unable to get ast from {source}"));
        let func = ir.functions.get("func").unwrap();
        let block = func.blocks.get(0).unwrap();
        match block.ops.last().unwrap() {
            IROp::Return { value } => {
                assert_eq!(
                    *value,
                    IRRegId::UNIT,
                    "context should be discarded in favor of builtin unit register R0"
                )
            }
            _ => panic!("return expected, not {source}"),
        }
    }

    r#impl("fn func(a1: (), a2:()){a1;}");
    r#impl("fn func(a1: (), a2:()){a2;}");
    r#impl("fn func(a1: (), a2:()){();a1;}");
    r#impl("fn func(a1: (), a2:()){();a2;}");
    r#impl("fn func(a1: (), a2:()){();();a1;}");
    r#impl("fn func(a1: (), a2:()){();();a2;}");
}
