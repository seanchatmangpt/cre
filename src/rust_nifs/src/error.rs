//! Error handling for CRE Rust NIF
//!
//! This module provides error types and conversion functions for
//! translating between Rust errors and Erlang terms.

use rustler::{Encoder, Env, Term};
use std::fmt;

/// NIF-specific error type
///
/// Errors are converted to Erlang `{error, Reason}` tuples.
#[derive(Debug, Clone)]
pub enum NifError {
    /// Invalid argument type or value
    BadArg,

    /// JSON parsing error
    Json(String),

    /// Resource not found (by ID)
    ResourceNotFound(usize),

    /// Operation timeout
    Timeout,

    /// Algorithm-specific error
    AlgorithmError(String),

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
            NifError::AlgorithmError(s) => write!(f, "algorithm_error: {}", s),
            NifError::Custom(s) => write!(f, "error: {}", s),
        }
    }
}

impl std::error::Error for NifError {}

impl From<rustler::Error> for NifError {
    fn from(err: rustler::Error) -> Self {
        match err {
            rustler::Error::BadArg => NifError::BadArg,
            rustler::Error::AtomAlreadyDefined => {
                NifError::Custom("atom_already_defined".to_string())
            }
            rustler::Error::NifInterfaceDisabled => {
                NifError::Custom("nif_interface_disabled".to_string())
            }
            _ => NifError::Custom(format!("rustler_error: {:?}", err)),
        }
    }
}

impl From<serde_json::Error> for NifError {
    fn from(err: serde_json::Error) -> Self {
        NifError::Json(format!("{}", err))
    }
}

impl<'a> Encoder for NifError {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let error_atom = rustler::Atom::try_from_str(env, "error").unwrap().to_term(env);

        let reason = match self {
            NifError::BadArg => {
                rustler::Atom::try_from_str(env, "badarg").unwrap().to_term(env)
            }
            NifError::Json(s) => {
                let msg = format!("json_error: {}", s);
                msg.encode(env)
            }
            NifError::ResourceNotFound(id) => {
                format!("resource_not_found: {}", id).encode(env)
            }
            NifError::Timeout => rustler::Atom::try_from_str(env, "timeout").unwrap().to_term(env),
            NifError::AlgorithmError(s) => format!("algorithm_error: {}", s).encode(env),
            NifError::Custom(s) => s.encode(env),
        };

        (error_atom, reason).encode(env)
    }
}

/// Convert a NifResult to an Erlang term result
///
/// This helper function converts a `Result<T, NifError>` into
/// an Erlang-style `{ok, Value}` or `{error, Reason}` tuple.
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

/// Macro for easy error conversion
///
/// # Examples
///
/// ```ignore
/// let result = nif_result!(some_operation());
/// ```
#[macro_export]
macro_rules! nif_result {
    ($expr:expr) => {
        $expr.map_err(|e| NifError::Custom(format!("{}", e)))
    };
}

/// Helper function to create a badarg error
pub fn badarg() -> NifError {
    NifError::BadArg
}

/// Helper function to create a custom error
pub fn custom_error(msg: impl Into<String>) -> NifError {
    NifError::Custom(msg.into())
}

/// Helper function to create a JSON error
pub fn json_error(msg: impl Into<String>) -> NifError {
    NifError::Json(msg.into())
}

/// Helper function to create an algorithm error
pub fn algorithm_error(msg: impl Into<String>) -> NifError {
    NifError::AlgorithmError(msg.into())
}

/// Helper function to create a resource not found error
pub fn resource_not_found(id: usize) -> NifError {
    NifError::ResourceNotFound(id)
}

/// Helper function to create a timeout error
pub fn timeout_error() -> NifError {
    NifError::Timeout
}

// ============================================================================
// Tests
// ============================================================================

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

    #[test]
    fn test_error_helpers() {
        let err = badarg();
        assert!(matches!(err, NifError::BadArg));

        let err = custom_error("custom");
        assert_eq!(format!("{}", err), "error: custom");

        let err = json_error("parse failed");
        assert_eq!(format!("{}", err), "json_error: parse failed");

        let err = algorithm_error("convergence failed");
        assert_eq!(format!("{}", err), "algorithm_error: convergence failed");

        let err = resource_not_found(123);
        assert_eq!(format!("{}", err), "resource_not_found: 123");

        let err = timeout_error();
        assert!(matches!(err, NifError::Timeout));
    }

    #[test]
    fn test_from_rustler_error() {
        let rustler_err = rustler::Error::BadArg;
        let nif_err: NifError = rustler_err.into();
        assert!(matches!(nif_err, NifError::BadArg));
    }

    #[test]
    fn test_from_json_error() {
        let json_err = serde_json::from_str::<serde_json::Value>("invalid json")
            .unwrap_err();
        let nif_err: NifError = json_err.into();
        assert!(matches!(nif_err, NifError::Json(_)));
    }
}
