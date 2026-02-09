//! LLM Process Modeling Library
//!
//! This library implements text-to-process model conversion using Large Language Models,
//! based on "Process Modeling With Large Language Models" by van der Aalst (2023).

pub mod mod;

pub use mod::*;
pub use crate::common::*;
pub use crate::algorithms::generative_ai::LLMClient;