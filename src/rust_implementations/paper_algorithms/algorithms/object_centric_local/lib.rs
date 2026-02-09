//! Object-Centric Local Process Models Library
//!
//! This library implements mining of local process models for object-centric event data,
//! based on "Object-Centric Local Process Models" by van der Aalst (2024).

pub mod mod;

pub use mod::*;
pub use crate::common::*;
pub use crate::algorithms::object_centric::OCELLog;