use std::collections::HashMap;

use crate::cst::{self, CST};

#[derive(Debug)]
struct TpFunctionArg {
    name: Option<String>,
    r#type: Type,
}

#[derive(Debug)]
struct TpFunction {
    args: Vec<TpFunctionArg>,
    return_type: Type,
}

#[derive(Debug)]
enum Type {
    Function(Box<TpFunction>),
    Unit,
}

#[derive(Debug)]
struct Function {
    name: String,
    r#type: TpFunction,
}

pub struct AST {
    functions: HashMap<String, TpFunction>,
}

impl AST {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
        }
    }

    pub fn from_cst(cst: CST) {
        // Prepare functions
        for cst_func in cst.functions.values() {
            unimplemented!("TBD")
        }
    }
}
