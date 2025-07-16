#[macro_export]
macro_rules! unwrap_variant {
    ($expr:expr, $variant:path) => {
        match $expr {
            $variant(val) => val,
            _ => unwrap_variant!(@ UNEXPECTED_KIND $expr, $variant)
        }
    };

    ($expr:expr, $variant:path, 2) => {
        match $expr {
            $variant(x,y) => (x,y),
            _ => unwrap_variant!(@ UNEXPECTED_KIND $expr, $variant)
        }
    };

    ($expr:expr, $variant:path, 3) => {
        match $expr {
            $variant(x,y,z) => (x,y,z),
            _ => unwrap_variant!(@ UNEXPECTED_KIND $expr, $variant)
        }
    };

    ($expr:expr, $variant:path, ()) => {
        match $expr {
            $variant => (),
            _ => unwrap_variant!(@ UNEXPECTED_KIND $expr, $variant)
        }
    };

    (@ UNEXPECTED_KIND $expr:expr, $variant:path) => {
        panic!(
            "Expected variant {} but got NodeKind::{:?}",
            stringify!($variant),
            $expr
        )
    }
}
