use crate::{
    IR,
    ir::{IRCodeBlockId, IROp, IRReg, IRTypeId},
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
        assert_eq!(r, IRReg(0));
    } else {
        panic!("ret expected");
    }
}

#[test]
fn test_arg_types() {
    let source = " fn main(none: ()) {} ";
    let ir = IR::from_thelan(source).unwrap();
    let func = ir.functions.get("main").unwrap();
    assert_eq!(func.args.len(), 1);
    assert_eq!(func.get_reg_type(func.args[0]), IRTypeId::UNIT);
    assert!(func.args[0].0 >= IRReg::BUILTIN_REGS_COUNT);
}

#[test]
fn test_nested_return() {
    let source = " fn main() { {()} } ";
    let ir = IR::from_thelan(source).unwrap();
    let expected = "\
FUNC main
.b0:
$r1 = call .b1
ret $r1

.b1:
ret $r0

END FUNC main\n";
    assert_eq!(ir.to_text(), expected);
}
