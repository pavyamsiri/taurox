use compact_str::CompactString;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ResolverErrorKind {
    #[error("Local variable {0} can not be accessed in its own initializer.")]
    InvalidLocalInitializer(CompactString),
}

#[derive(Debug, Error, Clone)]
#[error("({line}) {kind}")]
pub struct ResolverError {
    #[source]
    pub(crate) kind: ResolverErrorKind,
    pub(crate) line: u32,
}
