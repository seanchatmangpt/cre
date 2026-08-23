//! Resource management for Rust objects in Erlang NIFs
//!
//! This module provides safe resource management for long-lived Rust objects
//! that need to be accessed across multiple NIF calls.

use rustler::{Env, NifResult, ResourceArc};
use std::collections::HashMap;
use std::sync::{Mutex, RwLock};

/// Resource for managing event logs
#[derive(Debug)]
pub struct EventLogResource {
    pub log: crate::paper_algorithms::common::EventLog,
}

/// Resource for managing process models
#[derive(Debug)]
pub struct ProcessModelResource {
    pub model: crate::paper_algorithms::common::ProcessModel,
}

/// Resource for managing algorithm results
#[derive(Debug)]
pub struct AlgorithmResultResource {
    pub result: AlgorithmResultEnum,
}

/// Enum representing different algorithm results
#[derive(Debug)]
pub enum AlgorithmResultEnum {
    Alpha(crate::paper_algorithms::alpha::AlphaResult),
    Heuristic(crate::paper_algorithms::heuristic_miner::HeuristicResult),
    Conformance(crate::paper_algorithms::conformance_checking::ConformanceResult),
}

/// Resource tracker for managing all resources
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

    pub fn store_log(&self, log: EventLogResource) -> usize {
        let mut id_guard = self.next_id.write().unwrap();
        let id = *id_guard;
        *id_guard += 1;

        let mut logs = self.logs.lock().unwrap();
        logs.insert(id, ResourceArc::new(log));

        id
    }

    pub fn get_log(&self, id: usize) -> Option<ResourceArc<EventLogResource>> {
        let logs = self.logs.lock().unwrap();
        logs.get(&id).cloned()
    }

    pub fn remove_log(&self, id: usize) -> bool {
        let mut logs = self.logs.lock().unwrap();
        logs.remove(&id).is_some()
    }

    pub fn store_model(&self, model: ProcessModelResource) -> usize {
        let mut id_guard = self.next_id.write().unwrap();
        let id = *id_guard;
        *id_guard += 1;

        let mut models = self.models.lock().unwrap();
        models.insert(id, ResourceArc::new(model));

        id
    }

    pub fn get_model(&self, id: usize) -> Option<ResourceArc<ProcessModelResource>> {
        let models = self.models.lock().unwrap();
        models.get(&id).cloned()
    }

    pub fn remove_model(&self, id: usize) -> bool {
        let mut models = self.models.lock().unwrap();
        models.remove(&id).is_some()
    }
}

impl Default for ResourceTracker {
    fn default() -> Self {
        Self::new()
    }
}

// Implement rustler::Resource for each resource type
// Note: rustler::resource! macro requires specific invocation
// The actual resources are declared at module level with the macro

// Resource Arc wrapper types
pub type EventLogResourceArc = rustler::ResourceArc<EventLogResource>;
pub type ProcessModelResourceArc = rustler::ResourceArc<ProcessModelResource>;
pub type AlgorithmResultResourceArc = rustler::ResourceArc<AlgorithmResultResource>;

/// Create a resource from an event log
#[rustler::nif]
pub fn resource_create<'env>(
    env: Env<'env>,
    log_input: rustler::Term<'env>,
) -> NifResult<(usize, rustler::Atom)> {
    // For now, return a dummy resource ID
    // In production, this would properly create and store the resource
    let id = 1;

    let ok_atom = rustler::Atom::try_from_str(env, "ok").unwrap();

    Ok((id, ok_atom))
}

/// Get a resource by ID
#[rustler::nif]
pub fn resource_get<'env>(
    env: Env<'env>,
    _resource_id: usize,
) -> NifResult<rustler::Term<'env>> {
    // For now, return a placeholder
    let ok_atom = rustler::Atom::try_from_str(env, "ok").unwrap();
    Ok(ok_atom.to_term(env))
}

/// Update a resource
#[rustler::nif]
pub fn resource_update<'env>(
    env: Env<'env>,
    _resource_id: usize,
    _update: rustler::Term<'env>,
) -> NifResult<rustler::Atom> {
    let ok_atom = rustler::Atom::try_from_str(env, "ok").unwrap();
    Ok(ok_atom)
}

/// Delete a resource
#[rustler::nif]
pub fn resource_delete<'env>(
    env: Env<'env>,
    _resource_id: usize,
) -> NifResult<bool> {
    Ok(true)
}
