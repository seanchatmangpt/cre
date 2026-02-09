//! Common types, traits, and utilities for process mining algorithms
//!
//! This module provides foundational data structures and interfaces used across
//! all algorithm implementations in the paper-implementations library.

use std::collections::{HashMap, HashSet, BTreeMap};
use std::fmt;
use serde::{Serialize, Deserialize};
use petgraph::graph::{NodeIndex, UnGraph};
use ndarray::{Array2, Array1};
use smallvec::SmallVec;
use hashbrown::HashMap as BrownHashMap;

/// Event log entry with full XES 2.0 support
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Event {
    pub id: String,
    pub activity: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub case_id: String,
    pub resource: Option<String>,
    pub lifecycle: Option<String>,
    pub cost: Option<f64>,
    pub org_resource: Option<String>,
    pub group: Option<String>,
    pub concept_name: Option<String>,
    pub resource_role: Option<String>,
    pub application: Option<String>,
    pub variant_number: Option<i32>,
    pub follow_up_number: Option<i32>,
    pub time_passed: Option<chrono::Duration>,
    pub transition_code: Option<String>,
    pub case_priority: Option<String>,
    pub work_item_status: Option<String>,
    pub work_item_result: Option<String>,
    pub work_item_ref: Option<String>,
    pub workitem_type: Option<String>,
    pub milestone: Option<String>,
    pub milestone_reached: Option<String>,
    pub variant_id: Option<String>,
    pub case_name: Option<String>,
    pub in_file_name: Option<String>,
    pub simulation: Option<String>,
    pub semantics: Option<String>,
    pub case_service_time: Option<chrono::Duration>,
    pub case_waiting_time: Option<chrono::Duration>,
    pub case_complete_time: Option<chrono::Duration>,
    pub remaining_time: Option<chrono::Duration>,
    pub assigned_time: Option<chrono::Duration>,
    pub suspended_time: Option<chrono::Duration>,
    pub routing_time: Option<chrono::Duration>,
    pub other_attributes: HashMap<String, serde_json::Value>,
}

impl Event {
    /// Create a new minimal event
    pub fn new(case_id: String, activity: String, timestamp: chrono::DateTime<chrono::Utc>) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            activity,
            timestamp,
            case_id,
            resource: None,
            lifecycle: None,
            cost: None,
            org_resource: None,
            group: None,
            concept_name: None,
            resource_role: None,
            application: None,
            variant_number: None,
            follow_up_number: None,
            time_passed: None,
            transition_code: None,
            case_priority: None,
            work_item_status: None,
            work_item_result: None,
            work_item_ref: None,
            workitem_type: None,
            milestone: None,
            milestone_reached: None,
            variant_id: None,
            case_name: None,
            in_file_name: None,
            simulation: None,
            semantics: None,
            case_service_time: None,
            case_waiting_time: None,
            case_complete_time: None,
            remaining_time: None,
            assigned_time: None,
            suspended_time: None,
            routing_time: None,
            other_attributes: HashMap::new(),
        }
    }

    /// Add a custom attribute
    pub fn with_attribute<T: Into<String>>(mut self, key: T, value: serde_json::Value) -> Self {
        self.other_attributes.insert(key.into(), value);
        self
    }

    /// Get custom attribute
    pub fn get_attribute(&self, key: &str) -> Option<&serde_json::Value> {
        self.other_attributes.get(key)
    }

    /// Convert to XES string representation
    pub fn to_xes(&self) -> String {
        format!(
            r#"<event>
    <id>{}</id>
    <activity>{}</activity>
    <timestamp>{}</timestamp>
    <case_id>{}</case_id>
    <resource>{}</resource>
    <lifecycle>{}</lifecycle>
    <cost>{}</cost>
    <concept:name>{}</concept:name>
    <org:resource>{}</org:resource>
    <group>{}</group>
    <resource:role>{}</resource:role>
    <application>{}</application>
    <variant:variantNumber>{}</variant:variantNumber>
    <variant:followUpNumber>{}</variant:followUpNumber>
    <time:timestamp>{}</time:timestamp>
    <transition:code>{}</transition:code>
    <case:priority>{}</case:priority>
    <workItem:status>{}</workItem:status>
    <workItem:result>{}</workItem:result>
    <workItem:ref>{}</workItem:ref>
    <workItem:type>{}</workItem:type>
    <milestone>{}</milestone>
    <milestone:reached>{}</milestone:reached>
    <variant:id>{}</variant:id>
    <case:name>{}</case:name>
    <in:fileName>{}</in:fileName>
    <simulation>{}</simulation>
    <semantics>{}</semantics>
    <case:serviceTime>{}</case:serviceTime>
    <case:waitingTime>{}</case:waitingTime>
    <case:completeTime>{}</case:completeTime>
    <remainingTime>{}</remainingTime>
    <assignedTime>{}</assignedTime>
    <suspendedTime>{}</suspendedTime>
    <routingTime>{}</routingTime>
</event>"#,
            self.id,
            self.activity,
            self.timestamp.format("%Y-%m-%dT%H:%M:%S%.fZ"),
            self.case_id,
            self.resource.as_ref().unwrap_or(&"".to_string()),
            self.lifecycle.as_ref().unwrap_or(&"".to_string()),
            self.cost.as_ref().unwrap_or(&0.0),
            self.concept_name.as_ref().unwrap_or(&self.activity),
            self.org_resource.as_ref().unwrap_or(&"".to_string()),
            self.group.as_ref().unwrap_or(&"".to_string()),
            self.resource_role.as_ref().unwrap_or(&"".to_string()),
            self.application.as_ref().unwrap_or(&"".to_string()),
            self.variant_number.unwrap_or(0),
            self.follow_up_number.unwrap_or(0),
            self.timestamp.format("%Y-%m-%dT%H:%M:%S%.fZ"),
            self.transition_code.as_ref().unwrap_or(&"".to_string()),
            self.case_priority.as_ref().unwrap_or(&"".to_string()),
            self.work_item_status.as_ref().unwrap_or(&"".to_string()),
            self.work_item_result.as_ref().unwrap_or(&"".to_string()),
            self.work_item_ref.as_ref().unwrap_or(&"".to_string()),
            self.workitem_type.as_ref().unwrap_or(&"".to_string()),
            self.milestone.as_ref().unwrap_or(&"".to_string()),
            self.milestone_reached.as_ref().unwrap_or(&"".to_string()),
            self.variant_id.as_ref().unwrap_or(&"".to_string()),
            self.case_name.as_ref().unwrap_or(&"".to_string()),
            self.in_file_name.as_ref().unwrap_or(&"".to_string()),
            self.simulation.as_ref().unwrap_or(&"".to_string()),
            self.semantics.as_ref().unwrap_or(&"".to_string()),
            self.case_service_time.map_or("".to_string(), |d| format!("{:?}", d)),
            self.case_waiting_time.map_or("".to_string(), |d| format!("{:?}", d)),
            self.case_complete_time.map_or(""".to_string(), |d| format!("{:?}", d)),
            self.remaining_time.map_or("".to_string(), |d| format!("{:?}", d)),
            self.assigned_time.map_or("".to_string(), |d| format!("{:?}", d)),
            self.suspended_time.map_or("".to_string(), |d| format!("{:?}", d)),
            self.routing_time.map_or("".to_string(), |d| format!("{:?}", d))
        )
    }
}

/// Case (trace) representation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Case {
    pub id: String,
    pub name: Option<String>,
    pub events: Vec<Event>,
    pub attributes: HashMap<String, serde_json::Value>,
    pub start_time: Option<chrono::DateTime<chrono::Utc>>,
    pub end_time: Option<chrono::DateTime<chrono::Utc>>,
    pub duration: Option<chrono::Duration>,
    pub cost: Option<f64>,
    pub resource: Option<String>,
    pub variant: Option<String>,
}

impl Case {
    /// Create a new case
    pub fn new(id: String) -> Self {
        Self {
            id,
            name: None,
            events: Vec::new(),
            attributes: HashMap::new(),
            start_time: None,
            end_time: None,
            duration: None,
            cost: None,
            resource: None,
            variant: None,
        }
    }

    /// Add event to case
    pub fn add_event(&mut self, event: Event) {
        if self.start_time.is_none() || self.start_time.unwrap() > event.timestamp {
            self.start_time = Some(event.timestamp);
        }
        if self.end_time.is_none() || self.end_time.unwrap() < event.timestamp {
            self.end_time = Some(event.timestamp);
        }
        self.events.push(event);
        self.update_derived_attributes();
    }

    /// Update derived attributes
    fn update_derived_attributes(&mut self) {
        if let (Some(start), Some(end)) = (self.start_time, self.end_time) {
            self.duration = Some(end - start);
        }

        // Calculate case cost
        self.cost = self.events.iter()
            .filter_map(|e| e.cost)
            .sum::<f64>()
            .into();

        // Determine resource (most common resource)
        let resource_counts: HashMap<&String, usize> = self.events
            .iter()
            .filter_map(|e| e.resource.as_ref())
            .collect::<Vec<_>>()
            .into_iter()
            .counts();

        self.resource = resource_counts
            .into_iter()
            .max_by_key(|(_, count)| *count)
            .map(|(resource, _)| resource.clone());

        // Determine variant
        let variant: Option<String> = if self.events.len() > 1 {
            let activities: Vec<String> = self.events.iter()
                .map(|e| e.activity.clone())
                .collect();
            Some(activities.join(";"))
        } else {
            None
        };
        self.variant = variant;
    }

    /// Get sorted events by timestamp
    pub fn get_sorted_events(&self) -> &[Event] {
        // Events should be sorted, but just in case
        &self.events
    }

    /// Get activity sequence
    pub fn get_activity_sequence(&self) -> Vec<String> {
        self.events.iter()
            .map(|e| e.activity.clone())
            .collect()
    }

    /// Get case statistics
    pub fn get_statistics(&self) -> CaseStatistics {
        let num_events = self.events.len();
        let duration = self.duration.unwrap_or_default();
        let start_time = self.start_time;
        let end_time = self.end_time;
        let cost = self.cost.unwrap_or(0.0);

        let activity_counts: HashMap<String, usize> = self.events
            .iter()
            .map(|e| e.activity.clone())
            .collect::<Vec<_>>()
            .into_iter()
            .counts();

        let resource_counts: HashMap<String, usize> = self.events
            .iter()
            .filter_map(|e| e.resource.clone())
            .collect::<Vec<_>>()
            .into_iter()
            .counts();

        CaseStatistics {
            case_id: self.id.clone(),
            num_events,
            duration,
            start_time,
            end_time,
            cost,
            activity_counts,
            resource_counts,
        }
    }
}

/// Case statistics
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CaseStatistics {
    pub case_id: String,
    pub num_events: usize,
    pub duration: chrono::Duration,
    pub start_time: Option<chrono::DateTime<chrono::Utc>>,
    pub end_time: Option<chrono::DateTime<chrono::Utc>>,
    pub cost: f64,
    pub activity_counts: HashMap<String, usize>,
    pub resource_counts: HashMap<String, usize>,
}

/// Event log representation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct EventLog {
    pub id: String,
    pub name: Option<String>,
    pub description: Option<String>,
    pub source: Option<String>,
    pub encoding: Option<String>,
    pub target: Option<String>,
    pub format: Option<String>,
    pub classifiers: Vec<Classifier>,
    pub global_trace_attributes: HashMap<String, serde_json::Value>,
    pub global_event_attributes: HashMap<String, serde_json::Value>,
    pub extensions: HashMap<String, Extension>,
    pub types: HashMap<String, Type>,
    pub cases: HashMap<String, Case>,
    pub num_cases: usize,
    pub num_events: usize,
    pub activities: HashSet<String>,
    pub resources: HashSet<String>,
    pub timestamps_min: Option<chrono::DateTime<chrono::Utc>>,
    pub timestamps_max: Option<chrono::DateTime<chrono::Utc>>,
}

impl EventLog {
    /// Create a new event log
    pub fn new(id: String) -> Self {
        Self {
            id,
            name: None,
            description: None,
            source: None,
            encoding: Some("UTF-8".to_string()),
            target: None,
            format: Some("xes".to_string()),
            classifiers: Vec::new(),
            global_trace_attributes: HashMap::new(),
            global_event_attributes: HashMap::new(),
            extensions: HashMap::new(),
            types: HashMap::new(),
            cases: HashMap::new(),
            num_cases: 0,
            num_events: 0,
            activities: HashSet::new(),
            resources: HashSet::new(),
            timestamps_min: None,
            timestamps_max: None,
        }
    }

    /// Add a case to the event log
    pub fn add_case(&mut self, case: Case) -> Result<(), ProcessMiningError> {
        if self.cases.contains_key(&case.id) {
            return Err(ProcessMiningError::DuplicateCaseId(case.id));
        }

        // Update global statistics
        self.num_cases += 1;
        self.num_events += case.events.len();

        // Update activities and resources
        for event in &case.events {
            self.activities.insert(event.activity.clone());
            if let Some(resource) = &event.resource {
                self.resources.insert(resource.clone());
            }

            // Update timestamp range
            if let (Some(min), Some(max)) = (self.timestamps_min, self.timestamps_max) {
                if event.timestamp < min {
                    self.timestamps_min = Some(event.timestamp);
                }
                if event.timestamp > max {
                    self.timestamps_max = Some(event.timestamp);
                }
            } else {
                self.timestamps_min = Some(event.timestamp);
                self.timestamps_max = Some(event.timestamp);
            }
        }

        self.cases.insert(case.id.clone(), case);
        Ok(())
    }

    /// Add event to existing case
    pub fn add_event_to_case(&mut self, case_id: String, event: Event) -> Result<(), ProcessMiningError> {
        if let Some(case) = self.cases.get_mut(&case_id) {
            case.add_event(event);

            // Update global statistics
            self.num_events += 1;

            // Update activities and resources
            if !self.activities.contains(&event.activity) {
                self.activities.insert(event.activity.clone());
            }
            if let Some(resource) = &event.resource {
                if !self.resources.contains(resource) {
                    self.resources.insert(resource.clone());
                }
            }

            // Update timestamp range
            if let (Some(min), Some(max)) = (self.timestamps_min, self.timestamps_max) {
                if event.timestamp < min {
                    self.timestamps_min = Some(event.timestamp);
                }
                if event.timestamp > max {
                    self.timestamps_max = Some(event.timestamp);
                }
            } else {
                self.timestamps_min = Some(event.timestamp);
                self.timestamps_max = Some(event.timestamp);
            }

            Ok(())
        } else {
            Err(ProcessMiningError::CaseNotFound(case_id))
        }
    }

    /// Get all cases as a vector
    pub fn get_cases(&self) -> Vec<&Case> {
        self.cases.values().collect()
    }

    /// Get case by ID
    pub fn get_case(&self, case_id: &str) -> Option<&Case> {
        self.cases.get(case_id)
    }

    /// Get all events as a vector
    pub fn get_all_events(&self) -> Vec<&Event> {
        self.cases.values()
            .flat_map(|case| case.events.iter())
            .collect()
    }

    /// Get all activities sorted
    pub fn get_activities_sorted(&self) -> Vec<&String> {
        let mut activities: Vec<_> = self.activities.iter().collect();
        activities.sort();
        activities
    }

    /// Get all resources sorted
    pub fn get_resources_sorted(&self) -> Vec<&String> {
        let mut resources: Vec<_> = self.resources.iter().collect();
        resources.sort();
        resources
    }

    /// Get activity frequencies
    pub fn get_activity_frequencies(&self) -> HashMap<String, usize> {
        let mut frequencies = HashMap::new();
        for case in self.cases.values() {
            for event in &case.events {
                *frequencies.entry(event.activity.clone()).or_insert(0) += 1;
            }
        }
        frequencies
    }

    /// Get trace statistics
    pub fn get_trace_statistics(&self) -> Vec<CaseStatistics> {
        self.cases.values()
            .map(|case| case.get_statistics())
            .collect()
    }

    /// Get time range
    pub fn get_time_range(&self) -> Option<(chrono::DateTime<chrono::Utc>, chrono::DateTime<chrono::Utc>)> {
        match (self.timestamps_min, self.timestamps_max) {
            (Some(min), Some(max)) => Some((min, max)),
            _ => None,
        }
    }

    /// Get event log duration
    pub fn get_duration(&self) -> Option<chrono::Duration> {
        self.get_time_range()
            .map(|(start, end)| end - start)
    }

    /// Export to XES format
    pub fn to_xes(&self) -> Result<String, ProcessMiningError> {
        let mut xml = String::new();

        // Start XES root
        xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        xml.push_str("<log>\n");

        // Global extensions
        if !self.extensions.is_empty() {
            xml.push_str("  <extensions>\n");
            for ext in &self.extensions {
                xml.push_str(&format!("    <extension name=\"{}\" uri=\"{}\"/>\n", ext.0, ext.1.uri));
            }
            xml.push_str("  </extensions>\n");
        }

        // Global types
        if !self.types.is_empty() {
            xml.push_str("  <globals>\n");
            for typ in &self.types {
                xml.push_str(&format!("    <global name=\"{}\" key=\"{}\" type=\"{}\">\n", typ.0, typ.1.key, typ.1.type_name));
                if !typ.1.values.is_empty() {
                    for value in &typ.1.values {
                        xml.push_str(&format!("      <value>{}</value>\n", value));
                    }
                }
                xml.push_str("    </global>\n");
            }
            xml.push_str("  </globals>\n");
        }

        // Classifier definitions
        if !self.classifiers.is_empty() {
            xml.push_str("  <classifierDefinitions>\n");
            for classifier in &self.classifiers {
                xml.push_str(&format!("    <classifier name=\"{}\" keys=\"{}\"/>\n", classifier.name, classifier.keys.join(",")));
            }
            xml.push_str("  </classifierDefinitions>\n");
        }

        // Trace elements
        xml.push_str("  <trace>\n");
        for case in self.cases.values() {
            xml.push_str("    <trace>\n");
            xml.push_str(&format!("      <id>{}</id>\n", case.id));

            if let Some(name) = &case.name {
                xml.push_str(&format!("      <concept:name>{}</concept:name>\n", name));
            }

            // Global trace attributes
            for (key, value) in &case.attributes {
                xml.push_str(&format!("      <string key=\"{}\">{}</string>\n", key, value));
            }

            // Event elements
            for event in &case.events {
                xml.push_str(&event.to_xes());
            }

            xml.push_str("    </trace>\n");
        }
        xml.push_str("  </trace>\n");

        xml.push_str("</log>\n");

        Ok(xml)
    }
}

/// Classifier for event log classification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Classifier {
    pub name: String,
    pub keys: Vec<String>,
}

/// Extension definition
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Extension {
    pub name: String,
    pub uri: String,
    pub value_type: Option<String>,
}

/// Type definition
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Type {
    pub name: String,
    pub type_name: String,
    pub key: String,
    pub values: Vec<String>,
}

/// Process model representation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProcessModel {
    pub id: String,
    pub name: Option<String>,
    pub description: Option<String>,
    pub model_type: ModelType,
    pub graph: UnGraph<ProcessNode, ProcessEdge>,
    pub nodes: HashMap<String, NodeIndex>,
    pub start_nodes: Vec<NodeIndex>,
    pub end_nodes: Vec<NodeIndex>,
    pub activities: HashSet<String>,
    pub fitness: Option<f64>,
    pub precision: Option<f64>,
    pub generalization: Option<f64>,
    pub simplicity: Option<f64>,
}

/// Process node types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ProcessNodeType {
    Start,
    End,
    Activity(String),
    Gateway(GatewayType),
    Subprocess(String),
    Intermediate,
}

/// Gateway types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum GatewayType {
    Parallel,
    Exclusive,
    Conditional,
    Or,
    Data,
}

/// Process model types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ModelType {
    PetriNet,
    Yawl,
    BPMN,
    HeuristicsNet,
    AlphaNet,
    LocalProcessModel,
    ObjectCentric,
}

/// Process node
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProcessNode {
    pub id: String,
    pub name: Option<String>,
    pub node_type: ProcessNodeType,
    pub position: Option<(f64, f64)>,
    pub labels: Vec<String>,
    pub properties: HashMap<String, serde_json::Value>,
}

/// Process edge
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProcessEdge {
    pub id: String,
    pub source: NodeIndex,
    pub target: NodeIndex,
    pub weight: Option<f64>,
    pub conditions: Vec<String>,
    pub properties: HashMap<String, serde_json::Value>,
}

/// Petri net specific types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Place {
    pub id: String,
    pub name: Option<String>,
    pub marking: i32,
    pub capacity: Option<i32>,
    pub properties: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Transition {
    pub id: String,
    pub name: Option<String>,
    pub activity: Option<String>,
    pub is_invisible: bool,
    pub timing: Option<TransitionTiming>,
    pub properties: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TransitionTiming {
    pub min_duration: Option<chrono::Duration>,
    pub max_duration: Option<chrono::Duration>,
    pub distribution: Option<String>,
    pub parameters: HashMap<String, f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Arc {
    pub id: String,
    pub source: String,
    pub target: String,
    pub weight: Option<i32>,
    pub inscription: Option<String>,
    pub properties: HashMap<String, serde_json::Value>,
}

/// Marking representation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Marking {
    pub places: HashMap<String, i32>,
    pub total_tokens: i32,
}

/// Process mining algorithms result types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AlphaResult {
    pub model: ProcessModel,
    pub fitness: f64,
    pub precision: f64,
    pub generalization: f64,
    pub simplicity: f64,
    pub computation_time: std::time::Duration,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct HeuristicResult {
    pub model: ProcessModel,
    pub frequencies: HashMap<String, usize>,
    pub dependencies: HashMap<(String, String), f64>,
    pub dependencies_inv: HashMap<(String, String), f64>,
    pub fitness: f64,
    pub precision: f64,
    pub computation_time: std::time::Duration,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ConformanceResult {
    pub trace_fitness: Vec<TraceFitness>,
    pub total_fitness: f64,
    pub precision: f64,
    pub fitness: f64,
    pub alignments: Vec<Alignment>,
    pub computation_time: std::time::Duration,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TraceFitness {
    pub case_id: String,
    pub fitness: f64,
    pub deviations: Vec<Deviation>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Deviation {
    pub type: DeviationType,
    pub description: String,
    pub location: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum DeviationType {
    MissingActivity,
    ExtraActivity,
    WrongOrder,
    WrongActivity,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Alignment {
    pub trace_id: String,
    pub alignment: Vec<AlignmentMove>,
    pub cost: f64,
    pub fitness: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AlignmentMove {
    pub move_type: AlignmentMoveType,
    pub trace_element: Option<String>,
    pub model_element: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AlignmentMoveType {
    MoveOnTrace,
    MoveOnModel,
    SyncMove,
    LogMove,
    ModelMove,
}

/// Error types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ProcessMiningError {
    InvalidEventLog(String),
    CaseNotFound(String),
    DuplicateCaseId(String),
    InvalidModel(String),
    ComputationError(String),
    ParseError(String),
    ValidationError(String),
    IoError(String),
    TimeoutError(String),
}

impl std::fmt::Display for ProcessMiningError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProcessMiningError::InvalidEventLog(msg) => write!(f, "Invalid event log: {}", msg),
            ProcessMiningError::CaseNotFound(id) => write!(f, "Case not found: {}", id),
            ProcessMiningError::DuplicateCaseId(id) => write!(f, "Duplicate case ID: {}", id),
            ProcessMiningError::InvalidModel(msg) => write!(f, "Invalid model: {}", msg),
            ProcessMiningError::ComputationError(msg) => write!(f, "Computation error: {}", msg),
            ProcessMiningError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            ProcessMiningError::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            ProcessMiningError::IoError(msg) => write!(f, "I/O error: {}", msg),
            ProcessMiningError::TimeoutError(msg) => write!(f, "Timeout error: {}", msg),
        }
    }
}

impl std::error::Error for ProcessMiningError {}

/// Performance metrics
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PerformanceMetrics {
    pub throughput: f64,
    pub average_case_duration: chrono::Duration,
    pub processing_time: chrono::Duration,
    pub waiting_time: chrono::Duration,
    pub service_time: chrono::Duration,
    pub queue_length: f64,
    pub utilization: f64,
    pub idle_time: chrono::Duration,
    pub resource_utilization: HashMap<String, f64>,
    pub activity_durations: HashMap<String, chrono::Duration>,
    pub case_durations: HashMap<String, chrono::Duration>,
}

/// Configurable parameters
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AlgorithmParameters {
    pub alpha_threshold: f64,
    pub heuristic_threshold: f64,
    pub conformance_threshold: f64,
    pub parallel_workers: usize,
    pub timeout_ms: u64,
    pub max_model_size: usize,
    pub confidence_level: f64,
    pub significance_level: f64,
    pub max_iterations: usize,
    pub epsilon: f64,
    pub convergence_threshold: f64,
}

impl Default for AlgorithmParameters {
    fn default() -> Self {
        Self {
            alpha_threshold: 0.05,
            heuristic_threshold: 0.8,
            conformance_threshold: 0.9,
            parallel_workers: 0, // 0 means use all available CPUs
            timeout_ms: 300_000, // 5 minutes
            max_model_size: 1000,
            confidence_level: 0.95,
            significance_level: 0.05,
            max_iterations: 1000,
            epsilon: 1e-10,
            convergence_threshold: 1e-6,
        }
    }
}

/// Configuration manager
pub struct ConfigManager {
    pub params: AlgorithmParameters,
    pub logging_enabled: bool,
    pub verbose_logging: bool,
    pub performance_monitoring: bool,
    pub memory_tracking: bool,
    pub cache_enabled: bool,
    pub cache_size_mb: usize,
}

impl Default for ConfigManager {
    fn default() -> Self {
        Self {
            params: AlgorithmParameters::default(),
            logging_enabled: true,
            verbose_logging: false,
            performance_monitoring: true,
            memory_tracking: true,
            cache_enabled: true,
            cache_size_mb: 1024,
        }
    }
}

impl ConfigManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_params(mut self, params: AlgorithmParameters) -> Self {
        self.params = params;
        self
    }

    pub fn enable_logging(&mut self) {
        self.logging_enabled = true;
    }

    pub fn disable_logging(&mut self) {
        self.logging_enabled = false;
    }

    pub fn set_parallel_workers(&mut self, workers: usize) {
        self.params.parallel_workers = workers;
    }

    pub fn set_timeout(&mut self, ms: u64) {
        self.params.timeout_ms = ms;
    }
}

/// Performance monitoring
pub struct PerformanceMonitor {
    pub start_time: std::time::Instant,
    pub memory_usage: Vec<u64>,
    pub cpu_usage: Vec<f64>,
    pub disk_io: Vec<u64>,
    pub network_io: Vec<u64>,
}

impl PerformanceMonitor {
    pub fn new() -> Self {
        Self {
            start_time: std::time::Instant::now(),
            memory_usage: Vec::new(),
            cpu_usage: Vec::new(),
            disk_io: Vec::new(),
            network_io: Vec::new(),
        }
    }

    pub fn elapsed(&self) -> std::time::Duration {
        self.start_time.elapsed()
    }

    pub fn snapshot(&mut self) {
        // Memory usage in bytes
        self.memory_usage.push(
            std::process::Command::new("ps")
                .arg("-o")
                .arg("rss=")
                .arg("-p")
                .arg(std::process::id().to_string())
                .output()
                .map(|output| {
                    if let Ok(bytes) = output.stdout.strip_suffix(b"\n") {
                        bytes.parse::<u64>().unwrap_or(0) * 1024 // Convert to bytes
                    } else {
                        0
                    }
                })
                .unwrap_or(0)
        );

        // CPU usage (placeholder)
        self.cpu_usage.push(0.0);
    }

    pub fn get_memory_mb(&self) -> Option<u64> {
        self.memory_usage.last().copied().map(|bytes| bytes / 1024 / 1024)
    }

    pub fn get_memory_peak_mb(&self) -> u64 {
        self.memory_usage.iter().copied().max().unwrap_or(0) / 1024 / 1024
    }

    pub fn report(&self) -> String {
        format!(
            "Performance Report:\n  Duration: {:?}\n  Memory (Current): {:?} MB\n  Memory (Peak): {:?} MB\n  CPU Samples: {}",
            self.elapsed(),
            self.get_memory_mb().unwrap_or(0),
            self.get_memory_peak_mb(),
            self.cpu_usage.len()
        )
    }
}

/// Utility functions
pub mod utils {
    use super::*;

    /// Calculate Hamming distance between two strings
    pub fn hamming_distance(a: &str, b: &str) -> usize {
        let a_chars: Vec<char> = a.chars().collect();
        let b_chars: Vec<char> = b.chars().collect();

        let min_len = std::cmp::min(a_chars.len(), b_chars.len());
        let distance = a_chars.iter().zip(b_chars.iter())
            .take(min_len)
            .filter(|(a_char, b_char)| a_char != b_char)
            .count();

        distance + (a_chars.len() - b_chars.len()).abs()
    }

    /// Calculate edit distance (Levenshtein distance)
    pub fn edit_distance(a: &str, b: &str) -> usize {
        let a: Vec<char> = a.chars().collect();
        let b: Vec<char> = b.chars().collect();

        let mut dp = Array2::<usize>::zeros((a.len() + 1, b.len() + 1));

        for i in 0..=a.len() {
            dp[[i, 0]] = i;
        }

        for j in 0..=b.len() {
            dp[[0, j]] = j;
        }

        for i in 1..=a.len() {
            for j in 1..=b.len() {
                if a[i - 1] == b[j - 1] {
                    dp[[i, j]] = dp[[i - 1, j - 1]];
                } else {
                    dp[[i, j]] = std::cmp::min(
                        dp[[i - 1, j]] + 1,
                        std::cmp::min(
                            dp[[i, j - 1]] + 1,
                            dp[[i - 1, j - 1]] + 1
                        )
                    );
                }
            }
        }

        dp[[a.len(), b.len()]]
    }

    /// Calculate Jaccard similarity
    pub fn jaccard_similarity<T: Eq + std::hash::Hash + Clone>(a: &[T], b: &[T]) -> f64 {
        let set_a: HashSet<T> = a.iter().cloned().collect();
        let set_b: HashSet<T> = b.iter().cloned().collect();

        let intersection = set_a.intersection(&set_b).count();
        let union = set_a.union(&set_b).count();

        if union == 0 {
            return 0.0;
        }

        intersection as f64 / union as f64
    }

    /// Calculate cosine similarity
    pub fn cosine_similarity(a: &[f64], b: &[f64]) -> f64 {
        let dot_product: f64 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
        let norm_a = a.iter().map(|x| x * x).sum::<f64>().sqrt();
        let norm_b = b.iter().map(|x| x * x).sum::<f64>().sqrt();

        if norm_a == 0.0 || norm_b == 0.0 {
            return 0.0;
        }

        dot_product / (norm_a * norm_b)
    }

    /// Calculate statistical significance (p-value)
    pub fn calculate_p_value(observed: f64, expected: f64, variance: f64) -> f64 {
        if variance == 0.0 {
            return 0.0;
        }

        let z_score = (observed - expected) / variance.sqrt();
        // Using normal distribution CDF (cumulative distribution function)
        // This is a simplified version - in production use proper statistical library
        if z_score < 0.0 {
            // Lower tail
            0.5 * (1.0 + erf(z_score / std::f64::consts::SQRT_2))
        } else {
            // Upper tail
            0.5 * (1.0 - erf(z_score / std::f64::consts::SQRT_2))
        }
    }

    // Error function approximation
    fn erf(x: f64) -> f64 {
        // Using approximation for error function
        let a1 = 0.254829592;
        let a2 = -0.284496736;
        let a3 = 1.421413741;
        let a4 = -1.453152027;
        let a5 = 1.061405429;
        let p = 0.3275911;

        let sign = if x < 0.0 { -1.0 } else { 1.0 };
        let x = x.abs();

        let t = 1.0 / (1.0 + p * x);
        let y = 1.0 - ((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t * x.exp(-x * x);

        sign * y
    }

    /// Statistical utility functions
    pub mod statistics {
        use super::*;

        /// Calculate mean
        pub fn mean(data: &[f64]) -> f64 {
            if data.is_empty() {
                return 0.0;
            }
            data.iter().sum::<f64>() / data.len() as f64
        }

        /// Calculate median
        pub fn median(data: &mut [f64]) -> f64 {
            if data.is_empty() {
                return 0.0;
            }

            data.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

            let len = data.len();
            if len % 2 == 0 {
                (data[len / 2 - 1] + data[len / 2]) / 2.0
            } else {
                data[len / 2]
            }
        }

        /// Calculate standard deviation
        pub fn std_dev(data: &[f64]) -> f64 {
            if data.len() < 2 {
                return 0.0;
            }

            let mean = mean(data);
            let variance = data.iter()
                .map(|x| (x - mean).powi(2))
                .sum::<f64>() / (data.len() - 1) as f64;

            variance.sqrt()
        }

        /// Calculate percentiles
        pub fn percentile(data: &mut [f64], p: f64) -> f64 {
            if data.is_empty() || p < 0.0 || p > 100.0 {
                return 0.0;
            }

            data.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

            let index = (p / 100.0 * (data.len() - 1) as f64) as usize;
            data[index]
        }

        /// Calculate correlation coefficient
        pub fn correlation(a: &[f64], b: &[f64]) -> f64 {
            if a.len() != b.len() || a.len() < 2 {
                return 0.0;
            }

            let mean_a = mean(a);
            let mean_b = mean(b);

            let numerator: f64 = a.iter().zip(b.iter())
                .map(|(x, y)| (x - mean_a) * (y - mean_b))
                .sum();

            let denominator_a = a.iter()
                .map(|x| (x - mean_a).powi(2))
                .sum::<f64>().sqrt();

            let denominator_b = b.iter()
                .map(|x| (x - mean_b).powi(2))
                .sum::<f64>().sqrt();

            if denominator_a == 0.0 || denominator_b == 0.0 {
                return 0.0;
            }

            numerator / (denominator_a * denominator_b)
        }
    }
}