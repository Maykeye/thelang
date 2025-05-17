use crate::IR;
/// Trait for generating an actual code
pub trait CodeGen
where
    Self: Sized,
{
    /// Error state for the codegen that contains all sort of errors.
    type Error;
    fn from_ir(ir: &IR) -> Result<Self, Self::Error>;
    fn to_text(&self) -> Result<Vec<String>, Self::Error>;
    fn decode_error(error: &Self::Error) -> String;
}
