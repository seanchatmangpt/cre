//! Type conversion utilities between Erlang and Rust
//!
//! This module provides safe conversion functions for translating
//! between Erlang NIF terms and Rust types, with proper error handling.

use rustler::{Atom, Decoder, Encoder, Env, Error, NifResult, Term};
use std::collections::{HashMap, HashSet};

use crate::types::*;
use crate::error::NifError;

// ============================================================================
// Event Log Conversions
// ============================================================================

/// Decode an event log from an Erlang term
///
/// Tries multiple formats in order:
/// 1. Binary/string (JSON)
/// 2. List of traces
/// 3. Map (pre-parsed structure)
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
    if let Ok(wrapper) = term.decode::<EventLogWrapper>() {
        return Ok(EventLogInput::Parsed(EventLogData {
            id: wrapper.id,
            cases: HashMap::new(),
            activities: HashSet::new(),
        }));
    }

    Err(Error::BadArg)
}

/// Decode a list of traces from an Erlang term
///
/// Expects format: [[<<"a">>, <<"b">>], [<<"a">>, <<"c">>]]
pub fn decode_traces_list<'env>(term: Term<'env>) -> NifResult<Vec<Vec<String>>> {
    let list: Vec<Term> = term.decode()?;

    list.into_iter()
        .map(|trace_term| {
            let trace_list: Vec<Term> = trace_term.decode()?;
            trace_list.into_iter()
                .map(|activity_term| decode_activity(activity_term))
                .collect()
        })
        .collect()
}

/// Decode a single activity from an Erlang term
///
/// Accepts both strings and atoms
fn decode_activity<'env>(term: Term<'env>) -> NifResult<String> {
    // Try string first
    if let Ok(s) = term.decode::<String>() {
        return Ok(s);
    }

    // Try atom
    if let Ok(atom) = atom_to_string(term) {
        return Ok(atom);
    }

    // Try binary
    if let Ok(binary) = term.decode::<rustler::Binary>() {
        if let Ok(s) = std::str::from_utf8(binary.as_slice()) {
            return Ok(s.to_string());
        }
    }

    Err(Error::BadArg)
}

/// Convert an atom to its string representation
pub fn atom_to_string<'env>(term: Term<'env>) -> NifResult<String> {
    term.atom_to_string().map_err(|_| Error::BadArg)
}

/// Encode a string as an Erlang atom if possible, otherwise as a binary
pub fn encode_string_or_atom<'a>(env: Env<'a>, s: &str) -> Term<'a> {
    // Try to create an atom first
    if let Ok(atom) = Atom::try_from_str(env, s) {
        return atom.to_term(env);
    }

    // Fall back to binary/string
    s.encode(env)
}

// ============================================================================
// Process Model Conversions
// ============================================================================

/// Decode a process model from an Erlang term
pub fn decode_process_model<'env>(term: Term<'env>) -> NifResult<ProcessModelWrapper> {
    term.decode::<ProcessModelWrapper>()
}

/// Encode ordering relations for Alpha algorithm
pub fn encode_relations<'a>(
    env: Env<'a>,
    relations: &RelationSet,
) -> NifResult<Term<'a>> {
    let mut map = HashMap::new();

    let direct_succession: Vec<Term> = relations
        .direct_succession
        .iter()
        .map(|(a, b)| (a.encode(env), b.encode(env)).encode(env))
        .collect();

    let causality: Vec<Term> = relations
        .causality
        .iter()
        .map(|(a, b)| (a.encode(env), b.encode(env)).encode(env))
        .collect();

    let parallel: Vec<Term> = relations
        .parallel
        .iter()
        .map(|(a, b)| (a.encode(env), b.encode(env)).encode(env))
        .collect();

    let activities: Vec<Term> = relations
        .activities
        .iter()
        .map(|a| encode_string_or_atom(env, a))
        .collect();

    map.insert("direct_succession", direct_succession.encode(env));
    map.insert("causality", causality.encode(env));
    map.insert("parallel", parallel.encode(env));
    map.insert("activities", activities.encode(env));

    Ok(map.encode(env))
}

/// Ordering relations extracted from an event log
#[derive(Debug, Clone, Default)]
pub struct RelationSet {
    pub direct_succession: HashSet<(String, String)>,
    pub causality: HashSet<(String, String)>,
    pub parallel: HashSet<(String, String)>,
    pub activities: HashSet<String>,
}

// ============================================================================
// Algorithm Parameter Conversions
// ============================================================================

/// Decode and validate algorithm parameters
pub fn decode_params<T>(term: Term) -> NifResult<T>
where
    T: Default + Decoder<'static>,
{
    // Try to decode as specific type
    if let Ok(params) = term.decode::<T>() {
        return Ok(params);
    }

    // Return default if decoding fails
    Ok(T::default())
}

/// Create a map of default Alpha parameters
pub fn default_alpha_params_map<'a>(env: Env<'a>) -> HashMap<String, Term<'a>> {
    let mut params = HashMap::new();
    params.insert("alpha_threshold".to_string(), 0.05.encode(env));
    params.insert("fitness_threshold".to_string(), 0.8.encode(env));
    params.insert("precision_threshold".to_string(), 0.7.encode(env));
    params.insert("enable_pruning".to_string(), true.encode(env));
    params.insert("max_model_size".to_string(), 1000.encode(env));
    params
}

/// Create a map of default Heuristic parameters
pub fn default_heuristic_params_map<'a>(env: Env<'a>) -> HashMap<String, Term<'a>> {
    let mut params = HashMap::new();
    params.insert("dependency_threshold".to_string(), 0.8.encode(env));
    params.insert("AND_threshold".to_string(), 0.6.encode(env));
    params.insert("OR_threshold".to_string(), 0.6.encode(env));
    params.insert("XOR_threshold".to_string(), 0.7.encode(env));
    params.insert("enable_noise_reduction".to_string(), true.encode(env));
    params
}

// ============================================================================
// Result Encoding Helpers
// ============================================================================

/// Encode a successful algorithm result
pub fn encode_ok_result<'a, T>(env: Env<'a>, value: T) -> Term<'a>
where
    T: Encoder,
{
    let ok_atom = Atom::try_from_str(env, "ok").unwrap().to_term(env);
    (ok_atom, value).encode(env)
}

/// Encode an error result
pub fn encode_error<'a>(env: Env<'a>, error: NifError) -> Term<'a> {
    error.encode(env)
}

/// Encode an option as either a value or undefined
pub fn encode_option<'a, T>(env: Env<'a>, opt: &Option<T>) -> Term<'a>
where
    T: Encoder,
{
    match opt {
        Some(value) => value.encode(env),
        None => Atom::try_from_str(env, "undefined")
            .unwrap()
            .to_term(env),
    }
}

// ============================================================================
// Map Conversion Helpers
// ============================================================================

/// Extract a string value from a map term
pub fn map_get_string<'a>(map: &HashMap<String, Term<'a>>, key: &str) -> Option<String> {
    map.get(key).and_then(|t| t.decode::<String>().ok())
}

/// Extract a float value from a map term
pub fn map_get_float<'a>(map: &HashMap<String, Term<'a>>, key: &str) -> Option<f64> {
    map.get(key)
        .and_then(|t| t.decode::<f64>().or_else(|_| t.decode::<i64>().map(|i| i as f64)).ok())
}

/// Extract a boolean value from a map term
pub fn map_get_bool<'a>(map: &HashMap<String, Term<'a>>, key: &str) -> Option<bool> {
    map.get(key).and_then(|t| t.decode::<bool>().ok())
}

/// Extract a list value from a map term
pub fn map_get_list<'a, T: Decoder<'a>>(map: &HashMap<String, Term<'a>>, key: &str) -> Option<Vec<T>> {
    map.get(key).and_then(|t| t.decode::<Vec<T>>().ok())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_relation_set_default() {
        let relations = RelationSet::default();
        assert!(relations.direct_succession.is_empty());
        assert!(relations.activities.is_empty());
    }

    #[test]
    fn test_relation_set_operations() {
        let mut relations = RelationSet::default();
        relations.activities.insert("a".to_string());
        relations.activities.insert("b".to_string());
        relations.direct_succession.insert(("a".to_string(), "b".to_string()));

        assert_eq!(relations.activities.len(), 2);
        assert_eq!(relations.direct_succession.len(), 1);
    }

    #[test]
    fn test_alpha_params_default() {
        let env = unsafe { Env::new() };
        let params = default_alpha_params_map(env);
        assert_eq!(params.len(), 5);
        assert!(params.contains_key("alpha_threshold"));
    }

    #[test]
    fn test_heuristic_params_default() {
        let env = unsafe { Env::new() };
        let params = default_heuristic_params_map(env);
        assert_eq!(params.len(), 5);
        assert!(params.contains_key("dependency_threshold"));
    }
}
