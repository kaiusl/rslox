
#[derive(thiserror::Error, miette::Diagnostic)]
#[derive(Debug, Clone)]
pub enum InterpretError {
    #[error("Compile error")]
    Compile,

    #[error("Runtime error")]
    Runtime
}