//! Type conversions between Erlang and Rust
//!
//! This module provides safe conversion functions for translating
//! between Erlang NIF terms and Rust types.

use rustler::{Encoder, Env, Error, NifResult, Term};
use std::collections::{HashMap, HashSet};

use crate::paper_algorithms_placeholder::*;

/// Input type for event logs from Erlang
///
/// Event logs can be provided in multiple formats:
/// - JSON string representation
/// - List of trace lists
/// - Pre-parsed map structure
#[derive(Debug, Clone)]
pub enum EventLogInput {
    /// JSON string representation of XES log
    Json(String),

    /// List of traces, where each trace is a list of activities
    Traces(Vec<Vec<String>>),

    /// Pre-parsed event log structure
    Parsed(EventLog),
}

impl EventLogInput {
    /// Convert the input to an EventLog
    pub fn into_event_log(self) -> NifResult<EventLog> {
        match self {
            EventLogInput::Parsed(log) => Ok(log),
            EventLogInput::Json(_json) => {
                // Placeholder - in production, parse JSON
                Ok(EventLog {
                    id: "json_log".to_string(),
                    cases: HashMap::new(),
                    activities: HashSet::new(),
                    num_cases: 0,
                    num_events: 0,
                })
            },
            EventLogInput::Traces(_traces) => {
                // Placeholder - in production, parse traces
                Ok(EventLog {
                    id: "trace_log".to_string(),
                    cases: HashMap::new(),
                    activities: HashSet::new(),
                    num_cases: 0,
                    num_events: 0,
                })
            }
        }
    }
}

/// Decode an event log from an Erlang term
pub fn decode_event_log<'env>(term: Term<'env>) -> NifResult<EventLogInput> {
    // Try binary/string (JSON)
    if let Ok(json) = term.decode::<String>() {
        return Ok(EventLogInput::Json(json));
    }

    // Try list of traces
    if let Ok(traces) = decode_traces_list(term) {
        return Ok(EventLogInput::Traces(traces));
    }

    // Try map (pre-parsed structure)
    if let Ok(_log) = decode_event_log_map(term) {
        // Return placeholder
        return Ok(EventLogInput::Parsed(EventLog {
            id: "map_log".to_string(),
            cases: HashMap::new(),
            activities: HashSet::new(),
            num_cases: 0,
            num_events: 0,
        }));
    }

    Err(Error::BadArg)
}

/// Decode a list of traces from an Erlang term
fn decode_traces_list<'env>(term: Term<'env>) -> NifResult<Vec<Vec<String>>> {
    let list: Vec<Term> = term.decode()?;

    list.into_iter()
        .map(|trace_term| {
            let trace_list: Vec<Term> = trace_term.decode()?;
            trace_list.into_iter()
                .map(|activity_term| {
                    if let Ok(s) = activity_term.decode::<String>() {
                        Ok(s)
                    } else if let Ok(atom) = activity_term.atom_to_string() {
                        Ok(atom)
                    } else {
                        Err(Error::BadArg)
                    }
                })
                .collect()
        })
        .collect()
}

/// Decode an event log from a map structure
fn decode_event_log_map<'env>(_term: Term<'env>) -> NifResult<EventLog> {
    // Placeholder implementation
    Ok(EventLog {
        id: "map_log".to_string(),
        cases: HashMap::new(),
        activities: HashSet::new(),
        num_cases: 0,
        num_events: 0,
    })
}

/// Benchmark result structure
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    pub duration_ms: u64,
    pub memory_mb: usize,
    pub algorithm: String,
    pub result: Term,
}

impl Encoder for BenchmarkResult {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        let mut map = HashMap::new();

        map.insert("duration_ms", self.duration_ms.encode(env));
        map.insert("memory_mb", self.memory_mb.encode(env));
        map.insert("algorithm", self.algorithm.encode(env));
        map.insert("result", self.result);

        map.encode(env)
    }
}

/// Enum for algorithm results
pub enum AlgorithmResult {
    Alpha,
    Heuristic,
    Conformance,
    ObjectCentric,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_benchmark_result_encode() {
        let env = rustler::Env::new();
        let result = BenchmarkResult {
            duration_ms: 100,
            memory_mb: 10,
            algorithm: "test".to_string(),
            result: rustler::Atom::try_from_str(env, "ok").unwrap().to_term(env),
        };
        let _encoded = result.encode(env);
    }
}
