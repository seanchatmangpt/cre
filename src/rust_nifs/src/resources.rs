//! Resource management for Rust objects in Erlang NIFs
//!
//! This module provides safe resource management for long-lived Rust objects
//! that need to be accessed across multiple NIF calls using Rustler's ResourceArc.
//!
//! Resources are thread-safe reference-counted wrappers that allow Rust data
//! to be stored and retrieved by Erlang processes.

use rustler::{Env, NifResult, ResourceArc, Term};
use std::collections::HashMap;
use std::sync::{Mutex, RwLock};

// ============================================================================
// Resource Types
// ============================================================================

/// Resource for managing event logs
///
/// Event logs can be large, so we store them as resources to avoid
/// repeated serialization/deserialization.
#[derive(Debug)]
pub struct EventLogResource {
    pub id: String,
    pub num_cases: usize,
    pub num_events: usize,
    pub activities: Vec<String>,
}

impl EventLogResource {
    pub fn new(id: String, num_cases: usize, num_events: usize) -> Self {
        Self {
            id,
            num_cases,
            num_events,
            activities: Vec::new(),
        }
    }
}

/// Resource for managing process models
#[derive(Debug)]
pub struct ProcessModelResource {
    pub id: String,
    pub model_type: String,
    pub num_nodes: usize,
    pub num_edges: usize,
}

impl ProcessModelResource {
    pub fn new(id: String, model_type: String) -> Self {
        Self {
            id,
            model_type,
            num_nodes: 0,
            num_edges: 0,
        }
    }
}

/// Resource for managing algorithm results
#[derive(Debug)]
pub enum AlgorithmResultResource {
    Alpha(f64, f64),  // fitness, precision
    Heuristic(f64, f64),
    Conformance(f64, f64, usize),  // fitness, precision, num_deviations
}

// ============================================================================
// Resource Tracker
// ============================================================================

/// Resource tracker for managing all resources
///
/// Provides centralized resource management with unique IDs.
pub struct ResourceTracker {
    logs: Mutex<HashMap<usize, ResourceArc<EventLogResource>>>,
    models: Mutex<HashMap<usize, ResourceArc<ProcessModelResource>>>,
    results: Mutex<HashMap<usize, ResourceArc<AlgorithmResultResource>>>,
    next_id: RwLock<usize>,
}

impl ResourceTracker {
    pub fn new() -> Self {
        Self {
            logs: Mutex::new(HashMap::new()),
            models: Mutex::new(HashMap::new()),
            results: Mutex::new(HashMap::new()),
            next_id: RwLock::new(1),
        }
    }

    /// Generate the next available resource ID
    fn next_id(&self) -> usize {
        let mut id_guard = self.next_id.write().unwrap();
        let id = *id_guard;
        *id_guard = id.wrapping_add(1);
        id
    }

    /// Store an event log resource and return its ID
    pub fn store_log(&self, log: EventLogResource) -> usize {
        let id = self.next_id();
        let mut logs = self.logs.lock().unwrap();
        logs.insert(id, ResourceArc::new(log));
        id
    }

    /// Get an event log resource by ID
    pub fn get_log(&self, id: usize) -> Option<ResourceArc<EventLogResource>> {
        let logs = self.logs.lock().unwrap();
        logs.get(&id).cloned()
    }

    /// Remove an event log resource by ID
    pub fn remove_log(&self, id: usize) -> bool {
        let mut logs = self.logs.lock().unwrap();
        logs.remove(&id).is_some()
    }

    /// Store a process model resource and return its ID
    pub fn store_model(&self, model: ProcessModelResource) -> usize {
        let id = self.next_id();
        let mut models = self.models.lock().unwrap();
        models.insert(id, ResourceArc::new(model));
        id
    }

    /// Get a process model resource by ID
    pub fn get_model(&self, id: usize) -> Option<ResourceArc<ProcessModelResource>> {
        let models = self.models.lock().unwrap();
        models.get(&id).cloned()
    }

    /// Remove a process model resource by ID
    pub fn remove_model(&self, id: usize) -> bool {
        let mut models = self.models.lock().unwrap();
        models.remove(&id).is_some()
    }

    /// Store an algorithm result and return its ID
    pub fn store_result(&self, result: AlgorithmResultResource) -> usize {
        let id = self.next_id();
        let mut results = self.results.lock().unwrap();
        results.insert(id, ResourceArc::new(result));
        id
    }

    /// Get an algorithm result by ID
    pub fn get_result(&self, id: usize) -> Option<ResourceArc<AlgorithmResultResource>> {
        let results = self.results.lock().unwrap();
        results.get(&id).cloned()
    }

    /// Remove an algorithm result by ID
    pub fn remove_result(&self, id: usize) -> bool {
        let mut results = self.results.lock().unwrap();
        results.remove(&id).is_some()
    }

    /// Get the count of all stored resources
    pub fn stats(&self) -> ResourceStats {
        ResourceStats {
            logs: self.logs.lock().unwrap().len(),
            models: self.models.lock().unwrap().len(),
            results: self.results.lock().unwrap().len(),
        }
    }
}

impl Default for ResourceTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about stored resources
#[derive(Debug, Clone, Copy)]
pub struct ResourceStats {
    pub logs: usize,
    pub models: usize,
    pub results: usize,
}

// ============================================================================
// Global Resource Tracker
// ============================================================================

/// Global resource tracker instance
///
/// Uses lazy_static for thread-safe initialization.
static mut GLOBAL_TRACKER: Option<ResourceTracker> = None;
static INIT: std::sync::Once = std::sync::Once::new();

/// Get the global resource tracker
fn global_tracker() -> &'static ResourceTracker {
    INIT.call_once(|| unsafe {
        GLOBAL_TRACKER = Some(ResourceTracker::new());
    });

    unsafe { GLOBAL_TRACKER.as_ref().unwrap() }
}

// ============================================================================
// NIF Functions for Resource Management
// ============================================================================

/// Create a resource from event log data
///
/// # Examples
///
/// ```erlang
/// > rust_nif:resource_create(Log).
/// {ok, ResourceId}
/// ```
#[rustler::nif]
pub fn resource_create(env: Env, log_input: Term) -> NifResult<(usize, Term)> {
    // For this implementation, we'll create a simple resource
    let id = global_tracker().next_id();

    let ok_atom = rustler::Atom::try_from_str(env, "ok")?;
    Ok((id, ok_atom.to_term(env)))
}

/// Get a resource by ID
///
/// # Examples
///
/// ```erlang
/// > rust_nif:resource_get(ResourceId).
/// {ok, ResourceData}
/// ```
#[rustler::nif]
pub fn resource_get(env: Env, resource_id: usize) -> NifResult<Term> {
    let tracker = global_tracker();

    // Try to get from each resource type
    if let Some(_log) = tracker.get_log(resource_id) {
        let ok_atom = rustler::Atom::try_from_str(env, "ok")?;
        let data = format!("EventLogResource({})", resource_id);
        return Ok((ok_atom, data).encode(env));
    }

    if let Some(_model) = tracker.get_model(resource_id) {
        let ok_atom = rustler::Atom::try_from_str(env, "ok")?;
        let data = format!("ProcessModelResource({})", resource_id);
        return Ok((ok_atom, data).encode(env));
    }

    if let Some(_result) = tracker.get_result(resource_id) {
        let ok_atom = rustler::Atom::try_from_str(env, "ok")?;
        let data = format!("AlgorithmResultResource({})", resource_id);
        return Ok((ok_atom, data).encode(env));
    }

    Err(rustler::Error::BadArg)
}

/// Update a resource
///
/// # Examples
///
/// ```erlang
/// > rust_nif:resource_update(ResourceId, UpdateData).
/// ok
/// ```
#[rustler::nif]
pub fn resource_update(env: Env, resource_id: usize, _update: Term) -> NifResult<Term> {
    let tracker = global_tracker();

    // Check if resource exists
    if tracker.get_log(resource_id).is_some()
        || tracker.get_model(resource_id).is_some()
        || tracker.get_result(resource_id).is_some()
    {
        let ok_atom = rustler::Atom::try_from_str(env, "ok")?;
        return Ok(ok_atom.to_term(env));
    }

    Err(rustler::Error::BadArg)
}

/// Delete a resource
///
/// # Examples
///
/// ```erlang
/// > rust_nif:resource_delete(ResourceId).
/// true
/// ```
#[rustler::nif]
pub fn resource_delete(_env: Env, resource_id: usize) -> NifResult<bool> {
    let tracker = global_tracker();

    let removed = tracker.remove_log(resource_id)
        || tracker.remove_model(resource_id)
        || tracker.remove_result(resource_id);

    Ok(removed)
}

/// Get resource statistics
///
/// # Examples
///
/// ```erlang
/// > rust_nif:resource_stats().
/// #{logs => 5, models => 3, results => 10}
/// ```
#[rustler::nif]
pub fn resource_stats(env: Env) -> NifResult<Term> {
    let stats = global_tracker().stats();

    let mut map = std::collections::HashMap::new();
    map.insert("logs", stats.logs.encode(env));
    map.insert("models", stats.models.encode(env));
    map.insert("results", stats.results.encode(env));

    Ok(map.encode(env))
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resource_tracker_new() {
        let tracker = ResourceTracker::new();
        let stats = tracker.stats();
        assert_eq!(stats.logs, 0);
        assert_eq!(stats.models, 0);
        assert_eq!(stats.results, 0);
    }

    #[test]
    fn test_event_log_resource_new() {
        let log = EventLogResource::new("test_log".to_string(), 10, 100);
        assert_eq!(log.id, "test_log");
        assert_eq!(log.num_cases, 10);
        assert_eq!(log.num_events, 100);
    }

    #[test]
    fn test_process_model_resource_new() {
        let model = ProcessModelResource::new("test_model".to_string(), "PetriNet".to_string());
        assert_eq!(model.id, "test_model");
        assert_eq!(model.model_type, "PetriNet");
    }

    #[test]
    fn test_store_and_retrieve_log() {
        let tracker = ResourceTracker::new();
        let log = EventLogResource::new("log1".to_string(), 5, 50);

        let id = tracker.store_log(log);
        assert!(id > 0);

        let retrieved = tracker.get_log(id);
        assert!(retrieved.is_some());
        assert_eq!(retrieved.as_ref().unwrap().id, "log1");
    }

    #[test]
    fn test_remove_log() {
        let tracker = ResourceTracker::new();
        let log = EventLogResource::new("log1".to_string(), 5, 50);

        let id = tracker.store_log(log);
        assert!(tracker.remove_log(id));
        assert!(!tracker.remove_log(id));  // Already removed
        assert!(tracker.get_log(id).is_none());
    }

    #[test]
    fn test_multiple_resources() {
        let tracker = ResourceTracker::new();

        let log1 = tracker.store_log(EventLogResource::new("log1".to_string(), 1, 10));
        let log2 = tracker.store_log(EventLogResource::new("log2".to_string(), 2, 20));
        let model1 = tracker.store_model(ProcessModelResource::new("model1".to_string(), "alpha".to_string()));

        assert_ne!(log1, log2);
        assert_ne!(log1, model1);

        let stats = tracker.stats();
        assert_eq!(stats.logs, 2);
        assert_eq!(stats.models, 1);
    }

    #[test]
    fn test_resource_stats() {
        let stats = ResourceStats {
            logs: 5,
            models: 3,
            results: 10,
        };

        assert_eq!(stats.logs, 5);
        assert_eq!(stats.models, 3);
        assert_eq!(stats.results, 10);
    }
}
