//! Error handling for CRE Rust NIF
//!
//! This module provides error types and conversion functions for
//! translating between Rust errors and Erlang terms.

use rustler::{Encoder, Env, Term};
use std::fmt;

/// NIF-specific error type
#[derive(Debug)]
pub enum NifError {
    /// Invalid argument type
    BadArg,

    /// JSON parsing error
    Json(String),

    /// Resource not found
    ResourceNotFound(usize),

    /// Timeout
    Timeout,

    /// Custom error message
    Custom(String),
}

impl fmt::Display for NifError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NifError::BadArg => write!(f, "badarg"),
            NifError::Json(s) => write!(f, "json_error: {}", s),
            NifError::ResourceNotFound(id) => write!(f, "resource_not_found: {}", id),
            NifError::Timeout => write!(f, "timeout"),
            NifError::Custom(s) => write!(f, "error: {}", s),
        }
    }
}

impl From<rustler::Error> for NifError {
    fn from(err: rustler::Error) -> Self {
        match err {
            rustler::Error::BadArg => NifError::BadArg,
            rustler::Error::AtomAlreadyDefined => NifError::Custom("atom_already_defined".to_string()),
            rustler::Error::NifInterfaceDisabled => NifError::Custom("nif_interface_disabled".to_string()),
            _ => NifError::Custom(format!("rustler_error: {:?}", err)),
        }
    }
}

impl<'a> Encoder for NifError {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let error_atom = rustler::Atom::try_from_str(env, "error").unwrap().to_term(env);

        let reason = match self {
            NifError::BadArg => rustler::Atom::try_from_str(env, "badarg").unwrap().to_term(env),
            NifError::Json(s) => format!("json_error: {}", s).encode(env),
            NifError::ResourceNotFound(id) => format!("resource_not_found: {}", id).encode(env),
            NifError::Timeout => rustler::Atom::try_from_str(env, "timeout").unwrap().to_term(env),
            NifError::Custom(s) => s.encode(env),
        };

        (error_atom, reason).encode(env)
    }
}

/// Convert a NifResult to an Erlang term result
pub fn result_to_term<'a, T>(env: Env<'a>, result: Result<T, NifError>) -> Term<'a>
where
    T: Encoder,
{
    match result {
        Ok(value) => {
            let ok_atom = rustler::Atom::try_from_str(env, "ok").unwrap().to_term(env);
            (ok_atom, value).encode(env)
        }
        Err(error) => error.encode(env),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = NifError::BadArg;
        assert_eq!(format!("{}", err), "badarg");

        let err = NifError::Custom("test error".to_string());
        assert_eq!(format!("{}", err), "error: test error");
    }
}
