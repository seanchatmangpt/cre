//! Type definitions for CRE Rust NIF
//!
//! This module provides type-safe wrappers for Erlang terms and
//! defines common data structures used across the NIF interface.

use rustler::{Decoder, Encoder, Env, NifResult, Term};
use std::collections::{HashMap, HashSet};
use std::fmt;

// ============================================================================
// Event Log Types
// ============================================================================

/// Source of an event log
#[derive(Debug, Clone, PartialEq)]
pub enum EventLogSource {
    /// XES file path
    Xes(String),
    /// JSON data
    Json,
    /// In-memory traces
    Traces,
}

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
    Parsed(EventLogData),
}

/// Wrapper for event log data
#[derive(Debug, Clone)]
pub struct EventLogWrapper {
    /// Unique identifier for this log
    pub id: String,

    /// Number of cases in the log
    pub num_cases: usize,

    /// Number of total events
    pub num_events: usize,

    /// Source of the event log
    pub source: EventLogSource,
}

impl<'a> Decoder<'a> for EventLogWrapper {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        // Try to decode as a map
        if let Ok(map) = term.decode::<HashMap<String, Term>>() {
            let id = map
                .get("id")
                .and_then(|t| t.decode::<String>().ok())
                .unwrap_or_else(|| utils::generate_id());

            let num_cases = map
                .get("num_cases")
                .and_then(|t| t.decode::<usize>().ok())
                .unwrap_or(0);

            let num_events = map
                .get("num_events")
                .and_then(|t| t.decode::<usize>().ok())
                .unwrap_or(0);

            return Ok(EventLogWrapper {
                id,
                num_cases,
                num_events,
                source: EventLogSource::Traces,
            });
        }

        // Try to decode as a list of traces
        if let Ok(traces) = term.decode::<Vec<Vec<String>>>() {
            let num_events = traces.iter().map(|t| t.len()).sum();
            return Ok(EventLogWrapper {
                id: utils::generate_id(),
                num_cases: traces.len(),
                num_events,
                source: EventLogSource::Traces,
            });
        }

        Err(rustler::Error::BadArg)
    }
}

impl<'a> Encoder for EventLogWrapper {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut map = HashMap::new();
        map.insert("id", self.id.encode(env));
        map.insert("num_cases", self.num_cases.encode(env));
        map.insert("num_events", self.num_events.encode(env));
        map.insert("source", format!("{:?}", self.source).encode(env));
        map.encode(env)
    }
}

/// Internal event log data structure
#[derive(Debug, Clone)]
pub struct EventLogData {
    pub id: String,
    pub cases: HashMap<String, CaseData>,
    pub activities: HashSet<String>,
}

/// Case data containing events
#[derive(Debug, Clone)]
pub struct CaseData {
    pub id: String,
    pub events: Vec<EventData>,
}

/// Individual event
#[derive(Debug, Clone)]
pub struct EventData {
    pub case_id: String,
    pub activity: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

// ============================================================================
// Process Model Types
// ============================================================================

/// Type of process model
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ModelType {
    PetriNet,
    AlphaNet,
    HeuristicNet,
    ProcessTree,
}

/// Wrapper for process model data
#[derive(Debug, Clone)]
pub struct ProcessModelWrapper {
    pub id: String,
    pub model_type: ModelType,
    pub activities: HashSet<String>,
}

impl<'a> Decoder<'a> for ProcessModelWrapper {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if let Ok(map) = term.decode::<HashMap<String, Term>>() {
            let id = map
                .get("id")
                .and_then(|t| t.decode::<String>().ok())
                .unwrap_or_else(|| utils::generate_id());

            let model_type = map
                .get("type")
                .and_then(|t| t.decode::<String>().ok())
                .and_then(|s| match s.as_str() {
                    "petri_net" => Some(ModelType::PetriNet),
                    "alpha_net" => Some(ModelType::AlphaNet),
                    "heuristic_net" => Some(ModelType::HeuristicNet),
                    "process_tree" => Some(ModelType::ProcessTree),
                    _ => None,
                })
                .unwrap_or(ModelType::AlphaNet);

            let activities: HashSet<String> = map
                .get("activities")
                .and_then(|t| t.decode::<Vec<String>>().ok())
                .map(|v| v.into_iter().collect())
                .unwrap_or_default();

            return Ok(ProcessModelWrapper {
                id,
                model_type,
                activities,
            });
        }

        Err(rustler::Error::BadArg)
    }
}

impl<'a> Encoder for ProcessModelWrapper {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut map = HashMap::new();
        map.insert("id", self.id.encode(env));
        map.insert("type", format!("{:?}", self.model_type).encode(env));

        let activities_vec: Vec<String> = self.activities.iter().cloned().collect();
        map.insert("activities", activities_vec.encode(env));

        map.encode(env)
    }
}

// ============================================================================
// Result Types
// ============================================================================

/// Log statistics structure
#[derive(Debug, Clone)]
pub struct LogStatistics {
    pub num_cases: usize,
    pub num_events: usize,
    pub num_activities: usize,
}

impl<'a> Encoder for LogStatistics {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut map = HashMap::new();
        map.insert("num_cases", self.num_cases.encode(env));
        map.insert("num_events", self.num_events.encode(env));
        map.insert("num_activities", self.num_activities.encode(env));
        map.encode(env)
    }
}

/// Benchmark result structure
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    pub duration_ms: u64,
    pub memory_mb: usize,
    pub algorithm: String,
    pub success: bool,
}

impl<'a> Encoder for BenchmarkResult {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut map = HashMap::new();
        map.insert("duration_ms", self.duration_ms.encode(env));
        map.insert("memory_mb", self.memory_mb.encode(env));
        map.insert("algorithm", self.algorithm.encode(env));
        map.insert("success", self.success.encode(env));
        map.encode(env)
    }
}

/// Alpha algorithm result
#[derive(Debug, Clone)]
pub struct AlphaResult {
    pub fitness: f64,
    pub precision: f64,
    pub model: Option<ProcessModelWrapper>,
}

impl<'a> Encoder for AlphaResult {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut map = HashMap::new();
        map.insert("fitness", self.fitness.encode(env));
        map.insert("precision", self.precision.encode(env));

        if let Some(ref model) = self.model {
            map.insert("model", model.encode(env));
        }

        map.encode(env)
    }
}

/// Heuristic miner result
#[derive(Debug, Clone)]
pub struct HeuristicResult {
    pub fitness: f64,
    pub precision: f64,
    pub dependencies: Vec<(String, String, f64)>,
}

impl<'a> Encoder for HeuristicResult {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut map = HashMap::new();
        map.insert("fitness", self.fitness.encode(env));
        map.insert("precision", self.precision.encode(env));

        let deps: Vec<Term> = self
            .dependencies
            .iter()
            .map(|(a, b, w)| {
                let mut tuple_map = HashMap::new();
                tuple_map.insert("from", a.encode(env));
                tuple_map.insert("to", b.encode(env));
                tuple_map.insert("weight", w.encode(env));
                tuple_map.encode(env)
            })
            .collect();

        map.insert("dependencies", deps.encode(env));
        map.encode(env)
    }
}

/// Conformance checking result
#[derive(Debug, Clone)]
pub struct ConformanceResult {
    pub fitness: f64,
    pub precision: f64,
    pub generalization: f64,
    pub num_deviations: usize,
}

impl<'a> Encoder for ConformanceResult {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut map = HashMap::new();
        map.insert("fitness", self.fitness.encode(env));
        map.insert("precision", self.precision.encode(env));
        map.insert("generalization", self.generalization.encode(env));
        map.insert("num_deviations", self.num_deviations.encode(env));
        map.encode(env)
    }
}

/// Object-centric result
#[derive(Debug, Clone)]
pub struct ObjectCentricResult {
    pub object_models: HashMap<String, ProcessModelWrapper>,
    pub interactions: Vec<(String, String)>,
}

impl<'a> Encoder for ObjectCentricResult {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut map = HashMap::new();

        let models_map: HashMap<String, Term> = self
            .object_models
            .iter()
            .map(|(k, v)| (k.clone(), v.encode(env)))
            .collect();
        map.insert("object_models", models_map.encode(env));

        let interactions: Vec<Term> = self
            .interactions
            .iter()
            .map(|(a, b)| (a.encode(env), b.encode(env)).encode(env))
            .collect();
        map.insert("interactions", interactions.encode(env));

        map.encode(env)
    }
}

// ============================================================================
// Parameter Types
// ============================================================================

/// Alpha algorithm parameters
#[derive(Debug, Clone)]
pub struct AlphaParams {
    pub alpha_threshold: f64,
    pub enable_pruning: bool,
    pub fitness_threshold: f64,
}

impl Default for AlphaParams {
    fn default() -> Self {
        Self {
            alpha_threshold: 0.05,
            enable_pruning: true,
            fitness_threshold: 0.8,
        }
    }
}

impl<'a> Decoder<'a> for AlphaParams {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if let Ok(map) = term.decode::<HashMap<String, Term>>() {
            Ok(AlphaParams {
                alpha_threshold: map
                    .get("alpha_threshold")
                    .and_then(|t| t.decode::<f64>().ok())
                    .unwrap_or(0.05),
                enable_pruning: map
                    .get("enable_pruning")
                    .and_then(|t| t.decode::<bool>().ok())
                    .unwrap_or(true),
                fitness_threshold: map
                    .get("fitness_threshold")
                    .and_then(|t| t.decode::<f64>().ok())
                    .unwrap_or(0.8),
            })
        } else {
            Ok(AlphaParams::default())
        }
    }
}

/// Heuristic miner parameters
#[derive(Debug, Clone)]
pub struct HeuristicParams {
    pub dependency_threshold: f64,
    pub and_threshold: f64,
    pub or_threshold: f64,
    pub xor_threshold: f64,
}

impl Default for HeuristicParams {
    fn default() -> Self {
        Self {
            dependency_threshold: 0.8,
            and_threshold: 0.6,
            or_threshold: 0.6,
            xor_threshold: 0.7,
        }
    }
}

impl<'a> Decoder<'a> for HeuristicParams {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if let Ok(map) = term.decode::<HashMap<String, Term>>() {
            Ok(HeuristicParams {
                dependency_threshold: map
                    .get("dependency_threshold")
                    .and_then(|t| t.decode::<f64>().ok())
                    .unwrap_or(0.8),
                and_threshold: map
                    .get("and_threshold")
                    .and_then(|t| t.decode::<f64>().ok())
                    .unwrap_or(0.6),
                or_threshold: map
                    .get("or_threshold")
                    .and_then(|t| t.decode::<f64>().ok())
                    .unwrap_or(0.6),
                xor_threshold: map
                    .get("xor_threshold")
                    .and_then(|t| t.decode::<f64>().ok())
                    .unwrap_or(0.7),
            })
        } else {
            Ok(HeuristicParams::default())
        }
    }
}

// ============================================================================
// Algorithm Enum
// ============================================================================

/// Enum representing different algorithm results
#[derive(Debug, Clone)]
pub enum AlgorithmResult {
    Alpha(AlphaResult),
    Heuristic(HeuristicResult),
    Conformance(ConformanceResult),
    ObjectCentric(ObjectCentricResult),
}

impl fmt::Display for AlgorithmResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AlgorithmResult::Alpha(_) => write!(f, "Alpha"),
            AlgorithmResult::Heuristic(_) => write!(f, "Heuristic"),
            AlgorithmResult::Conformance(_) => write!(f, "Conformance"),
            AlgorithmResult::ObjectCentric(_) => write!(f, "ObjectCentric"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alpha_params_default() {
        let params = AlphaParams::default();
        assert_eq!(params.alpha_threshold, 0.05);
        assert!(params.enable_pruning);
    }

    #[test]
    fn test_heuristic_params_default() {
        let params = HeuristicParams::default();
        assert_eq!(params.dependency_threshold, 0.8);
        assert_eq!(params.and_threshold, 0.6);
    }

    #[test]
    fn test_log_statistics_encode() {
        let stats = LogStatistics {
            num_cases: 10,
            num_events: 100,
            num_activities: 5,
        };
        // Test encoding - in real scenario, would verify term output
        assert_eq!(stats.num_cases, 10);
    }

    #[test]
    fn test_alpha_result() {
        let result = AlphaResult {
            fitness: 0.9,
            precision: 0.85,
            model: None,
        };
        assert_eq!(result.fitness, 0.9);
        assert_eq!(result.precision, 0.85);
    }
}
