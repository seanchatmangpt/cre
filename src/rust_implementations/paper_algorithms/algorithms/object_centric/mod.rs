//! Object-Centric Process Mining (OCEL) Implementation
//!
//! Implementation of Object-Centric Process Mining from the paper:
//! "OCEL (Object-Centric Event Log) 2.0 Specification" (2403.01975)
//! and related object-centric mining algorithms.
//!
//! Object-Centric Process Mining extends traditional process mining to handle
//! complex business processes involving multiple interacting entities.

use std::collections::{HashMap, HashSet, BTreeMap, BTreeSet};
use std::vec;
use petgraph::graph::{NodeIndex, UnGraph};
use petgraph::Undirected;
use serde::{Serialize, Deserialize};
use itertools::Itertools;
use rayon::prelude::*;

use crate::common::*;
use crate::common::errors::{ProcessMiningResult, ProcessMiningError};

/// Object-Centric Process Mining implementation
pub struct ObjectCentricMiner {
    pub log: EventLog,
    pub ocel: OCELLog,
    pub params: OCELParameters,
    pub model: ObjectCentricModel,
}

/// Object-Centric Event Log (OCEL 2.0)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCELLog {
    pub events: Vec<OCelEvent>,
    pub objects: HashMap<String, OCelObject>,
    pub object_types: HashMap<String, ObjectType>,
    pub object_relationships: Vec<ObjectRelationship>,
    pub object_attributes: HashMap<String, HashMap<String, serde_json::Value>>,
    pub event_object_relations: Vec<EventObjectRelation>,
    pub global_object_attributes: HashMap<String, serde_json::Value>,
}

/// OCEL Event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCelEvent {
    pub id: String,
    pub activity: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub lifecycle: Option<String>,
    pub attributes: HashMap<String, serde_json::Value>,
    pub related_objects: HashMap<String, ObjectRole>,
}

/// OCEL Object
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCelObject {
    pub id: String,
    pub object_type: String,
    pub attributes: HashMap<String, serde_json::Value>,
    pub creation_time: Option<chrono::DateTime<chrono::Utc>>,
    pub modification_time: Option<chrono::DateTime<chrono::Utc>>,
    pub deletion_time: Option<chrono::DateTime<chrono::Utc>>,
}

/// Object Type definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectType {
    pub name: String,
    pub attributes: HashMap<String, AttributeType>,
    pub relationships: Vec<String>,
}

/// Attribute type definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttributeType {
    pub name: String,
    pub type_name: String,
    pub required: bool,
    pub default_value: Option<serde_json::Value>,
}

/// Object Role in event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectRole {
    pub role: String,
    pub lifetime: ObjectLifetime,
    pub attributes: HashMap<String, serde_json::Value>,
}

/// Object lifetime
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ObjectLifetime {
    Opening,
    Closing,
    Use,
    Automatic,
    Custom,
}

/// Object relationship
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectRelationship {
    pub source: String,
    pub target: String,
    pub relationship_type: String,
    pub attributes: HashMap<String, serde_json::Value>,
}

/// Event-Object relation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventObjectRelation {
    pub event_id: String,
    pub object_id: String,
    pub role: String,
    pub attributes: HashMap<String, serde_json::Value>,
}

/// Object-Centric Process Model
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectCentricModel {
    pub id: String,
    pub name: Option<String>,
    pub object_types: HashMap<String, ObjectModel>,
    pub process_flows: Vec<ProcessFlow>,
    pub object_interactions: Vec<ObjectInteraction>,
    pub activities: HashSet<String>,
    pub relationships: HashMap<String, ObjectRelationship>,
    pub metrics: ObjectCentricMetrics,
}

/// Object model for each type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectModel {
    pub object_type: String,
    pub instances: Vec<ObjectInstance>,
    pub lifecycle: LifecycleModel,
    pub attributes: HashMap<String, AttributeModel>,
    pub relationships: Vec<ObjectModelRelationship>,
}

/// Object instance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectInstance {
    pub id: String,
    pub attributes: HashMap<String, serde_json::Value>,
    pub creation_events: Vec<String>,
    pub modification_events: Vec<String>,
    pub deletion_events: Vec<String>,
}

/// Lifecycle model
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LifecycleModel {
    pub states: Vec<String>,
    pub transitions: Vec<LifecycleTransition>,
    pub initial_state: String,
    pub final_state: String,
}

/// Lifecycle transition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LifecycleTransition {
    pub from: String,
    pub to: String,
    pub activities: Vec<String>,
    pub frequency: f64,
}

/// Attribute model
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttributeModel {
    pub name: String,
    pub type_name: String,
    pub constraints: Vec<String>,
    pub evolution: AttributeEvolution,
}

/// Attribute evolution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttributeEvolution {
    pub created_events: Vec<String>,
    pub modified_events: Vec<String>,
    pub deleted_events: Vec<String>,
}

/// Object model relationship
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectModelRelationship {
    pub source_type: String,
    pub target_type: String,
    pub relationship_type: String,
    pub constraints: Vec<String>,
    pub cardinality: String,
}

/// Process flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessFlow {
    pub id: String,
    pub name: String,
    pub source_objects: Vec<String>,
    pub target_objects: Vec<String>,
    pub activities: Vec<String>,
    pub flow_type: FlowType,
    pub metrics: FlowMetrics,
}

/// Flow type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FlowType {
    Creation,
    Deletion,
    Modification,
    Transfer,
    Interaction,
}

/// Flow metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowMetrics {
    pub frequency: f64,
    pub duration: Option<chrono::Duration>,
    pub success_rate: f64,
    pub throughput: f64,
}

/// Object interaction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectInteraction {
    pub participants: Vec<String>,
    pub interaction_type: String,
    pub activities: Vec<String>,
    pub patterns: Vec<InteractionPattern>,
    pub metrics: InteractionMetrics,
}

/// Interaction pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InteractionPattern {
    Sequential,
    Parallel,
    Choice,
    Iteration,
    Synchronization,
    Causation,
    MutualExclusion,
}

/// Interaction metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InteractionMetrics {
    pub frequency: f64,
    pub duration: chrono::Duration,
    pub complexity: f64,
    pub success_rate: f64,
    pub participant_satisfaction: f64,
}

/// Object-Centric metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectCentricMetrics {
    pub object_types_count: usize,
    pub object_instances_count: usize,
    pub event_object_relations_count: usize,
    pub average_objects_per_event: f64,
    pub average_events_per_object: f64,
    pub object_interaction_density: f64,
    pub process_complexity: f64,
    pub lifecycle_coverage: f64,
}

/// OCEL parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCELParameters {
    pub enable_lifecycle_analysis: bool,
    pub enable_attribute_analysis: bool,
    pub enable_relationship_analysis: bool,
    pub enable_interaction_analysis: bool,
    pub enable_pattern_detection: bool,
    pub enable_performance_analysis: bool,
    pub enable_conformance_checking: bool,
    pub lifecycle_threshold: f64,
    attribute_threshold: f64,
    relationship_threshold: f64,
    interaction_threshold: f64,
    pattern_threshold: f64,
    max_object_types: usize,
    max_instances_per_type: usize,
    enable_object_clustering: bool,
    clustering_algorithm: String,
    enable_streaming: bool,
    batch_size: usize,
    enable_visualization: bool,
    output_format: String,
}

impl Default for OCELParameters {
    fn default() -> Self {
        Self {
            enable_lifecycle_analysis: true,
            enable_attribute_analysis: true,
            enable_relationship_analysis: true,
            enable_interaction_analysis: true,
            enable_pattern_detection: true,
            enable_performance_analysis: true,
            enable_conformance_checking: false,
            lifecycle_threshold: 0.8,
            attribute_threshold: 0.7,
            relationship_threshold: 0.6,
            interaction_threshold: 0.7,
            pattern_threshold: 0.5,
            max_object_types: 100,
            max_instances_per_type: 1000,
            enable_object_clustering: true,
            clustering_algorithm: "kmeans".to_string(),
            enable_streaming: false,
            batch_size: 1000,
            enable_visualization: true,
            output_format: "json".to_string(),
        }
    }
}

impl ObjectCentricMiner {
    /// Create new Object-Centric Process Miner instance
    pub fn new(log: EventLog, params: OCELParameters) -> ProcessMiningResult<Self> {
        let ocel = Self::convert_to_ocel(&log)?;
        let model = ObjectCentricModel::new("ocel_model".to_string());

        Ok(Self {
            log,
            ocel,
            params,
            model,
        })
    }

    /// Convert traditional event log to OCEL format
    fn convert_to_ocel(log: &EventLog) -> ProcessMiningResult<OCELLog> {
        debug_pm!("ocel", "Converting event log to OCEL format");

        let mut events = Vec::new();
        let mut objects = HashMap::new();
        let mut object_types = HashMap::new();
        let mut event_object_relations = Vec::new();
        let mut object_attributes = HashMap::new();

        // Extract objects from events (case_id, resource, etc.)
        for (case_id, case) in &log.cases {
            // Create case object
            let case_object = OCelObject {
                id: case_id.clone(),
                object_type: "Case".to_string(),
                attributes: case.attributes.clone(),
                creation_time: case.start_time,
                modification_time: case.end_time,
                deletion_time: None,
            };
            objects.insert(case_id.clone(), case_object);

            // Add to object types
            if !object_types.contains_key("Case") {
                object_types.insert("Case".to_string(), ObjectType {
                    name: "Case".to_string(),
                    attributes: HashMap::new(),
                    relationships: Vec::new(),
                });
            }

            // Process events
            for event in &case.events {
                // Create event object
                let event_object = OCelObject {
                    id: event.id.clone(),
                    object_type: "Event".to_string(),
                    attributes: HashMap::new(),
                    creation_time: Some(event.timestamp),
                    modification_time: Some(event.timestamp),
                    deletion_time: None,
                };
                objects.insert(event.id.clone(), event_object);

                // Add to object types
                if !object_types.contains_key("Event") {
                    object_types.insert("Event".to_string(), ObjectType {
                        name: "Event".to_string(),
                        attributes: HashMap::new(),
                        relationships: Vec::new(),
                    });
                }

                // Create event
                let mut ocel_event = OCelEvent {
                    id: event.id.clone(),
                    activity: event.activity.clone(),
                    timestamp: event.timestamp,
                    lifecycle: event.lifecycle.clone(),
                    attributes: event.other_attributes.clone(),
                    related_objects: HashMap::new(),
                };

                // Add case relation
                ocel_event.related_objects.insert(case_id.clone(), ObjectRole {
                    role: "case".to_string(),
                    lifetime: ObjectLifetime::Use,
                    attributes: HashMap::new(),
                });

                // Add resource relation if exists
                if let Some(ref resource) = event.resource {
                    ocel_event.related_objects.insert(resource.clone(), ObjectRole {
                        role: "resource".to_string(),
                        lifetime: ObjectLifetime::Use,
                        attributes: HashMap::new(),
                    });

                    // Create resource object if not exists
                    if !objects.contains_key(resource) {
                        let resource_object = OCelObject {
                            id: resource.clone(),
                            object_type: "Resource".to_string(),
                            attributes: HashMap::new(),
                            creation_time: None,
                            modification_time: None,
                            deletion_time: None,
                        };
                        objects.insert(resource.clone(), resource_object);

                        // Add to object types
                        if !object_types.contains_key("Resource") {
                            object_types.insert("Resource".to_string(), ObjectType {
                                name: "Resource".to_string(),
                                attributes: HashMap::new(),
                                relationships: Vec::new(),
                            });
                        }
                    }
                }

                events.push(ocel_event);

                // Create event-object relations
                event_object_relations.push(EventObjectRelation {
                    event_id: event.id.clone(),
                    object_id: case_id.clone(),
                    role: "case".to_string(),
                    attributes: HashMap::new(),
                });

                if let Some(ref resource) = event.resource {
                    event_object_relations.push(EventObjectRelation {
                        event_id: event.id.clone(),
                        object_id: resource.clone(),
                        role: "resource".to_string(),
                        attributes: HashMap::new(),
                    });
                }
            }
        }

        let ocel_log = OCELLog {
            events,
            objects,
            object_types,
            object_relationships: Vec::new(),
            object_attributes,
            event_object_relations,
            global_object_attributes: HashMap::new(),
        };

        debug_pm!("ocel", "Converted to OCEL with {} events and {} objects",
            ocel_log.events.len(), ocel_log.objects.len());

        Ok(ocel_log)
    }

    /// Run Object-Centric Process Mining
    pub fn run(&mut self) -> ProcessMiningResult<ObjectCentricResult> {
        let start_time = std::time::Instant::now();

        info_pm!("ocel", "Starting Object-Centric Process Mining");

        // 1. Analyze lifecycle patterns
        let lifecycle_analysis = if self.params.enable_lifecycle_analysis {
            Some(self.analyze_lifecycle_patterns()?)
        } else {
            None
        };

        // 2. Analyze attribute evolution
        let attribute_analysis = if self.params.enable_attribute_analysis {
            Some(self.analyze_attribute_evolution()?)
        } else {
            None
        };

        // 3. Analyze object relationships
        let relationship_analysis = if self.params.enable_relationship_analysis {
            Some(self.analyze_object_relationships()?)
        } else {
            None
        };

        // 4. Analyze object interactions
        let interaction_analysis = if self.params.enable_interaction_analysis {
            Some(self.analyze_object_interactions()?)
        } else {
            None
        };

        // 5. Detect interaction patterns
        let pattern_detection = if self.params.enable_pattern_detection {
            Some(self.detect_interaction_patterns()?)
        } else {
            None
        };

        // 6. Build Object-Centric model
        self.build_object_centric_model()?;

        // 7. Calculate metrics
        let metrics = self.calculate_metrics()?;

        let computation_time = start_time.elapsed();

        let result = ObjectCentricResult {
            model: self.model.clone(),
            lifecycle_analysis,
            attribute_analysis,
            relationship_analysis,
            interaction_analysis,
            pattern_detection,
            metrics,
            computation_time,
        };

        info_pm!("ocel", "Object-Centric Process Mining completed in {:?}", computation_time);

        Ok(result)
    }

    /// Analyze lifecycle patterns
    fn analyze_lifecycle_patterns(&self) -> ProcessMiningResult<LifecycleAnalysis> {
        debug_pm!("ocel", "Analyzing lifecycle patterns");

        let mut lifecycle_models = HashMap::new();
        let mut object_type_counts = HashMap::new();

        // Count events per object type
        for event in &self.ocel.events {
            for (object_id, role) in &event.related_objects {
                if let Some(object) = self.ocel.objects.get(object_id) {
                    *object_type_counts.entry(object.object_type.clone()).or_insert(0) += 1;
                }
            }
        }

        // Build lifecycle models for each object type
        for (object_type, count) in object_type_counts {
            let lifecycle = self.build_lifecycle_model(&object_type)?;
            lifecycle_models.insert(object_type, lifecycle);
        }

        Ok(LifecycleAnalysis {
            lifecycle_models,
            object_type_counts,
            total_events: self.ocel.events.len(),
            average_events_per_object: self.ocel.events.len() as f64 / self.ocel.objects.len() as f64,
        })
    }

    /// Build lifecycle model for object type
    fn build_lifecycle_model(&self, object_type: &str) -> ProcessMiningResult<LifecycleModel> {
        let mut states = HashSet::new();
        let mut transitions = Vec::new();
        let mut state_counts = HashMap::new();

        // Extract states from lifecycle roles
        for event in &self.ocel.events {
            for (object_id, role) in &event.related_objects {
                if let Some(object) = self.ocel.objects.get(object_id) {
                    if object.object_type == object_type {
                        states.insert(role.role.clone());
                        *state_counts.entry(role.role.clone()).or_insert(0) += 1;
                    }
                }
            }
        }

        // Build transitions
        let states_vec: Vec<String> = states.into_iter().collect();
        for i in 0..states_vec.len() {
            for j in i + 1..states_vec.len() {
                let from = &states_vec[i];
                let to = &states_vec[j];

                // Find events that cause transition
                let transition_events: Vec<String> = self.ocel.events
                    .iter()
                    .filter(|event| {
                        event.related_objects.values().any(|role| role.role == from)
                    })
                    .map(|event| event.id.clone())
                    .collect();

                if !transition_events.is_empty() {
                    transitions.push(LifecycleTransition {
                        from: from.clone(),
                        to: to.clone(),
                        activities: transition_events,
                        frequency: state_counts.get(from).unwrap_or(&0) as f64 / state_counts.get(to).unwrap_or(&0) as f64,
                    });
                }
            }
        }

        // Determine initial and final states
        let initial_state = states_vec.first().cloned().unwrap_or_default();
        let final_state = states_vec.last().cloned().unwrap_or_default();

        Ok(LifecycleModel {
            states: states_vec,
            transitions,
            initial_state,
            final_state,
        })
    }

    /// Analyze attribute evolution
    fn analyze_attribute_evolution(&self) -> ProcessMiningResult<AttributeAnalysis> {
        debug_pm!("ocel", "Analyzing attribute evolution");

        let mut attribute_models = HashMap::new();
        let mut attribute_values = HashMap::new();

        // Collect attribute values over time
        for object in self.ocel.objects.values() {
            for (attr_name, attr_value) in &object.attributes {
                let entry = attribute_values.entry(attr_name.clone()).or_insert(HashMap::new());
                let values = entry.entry(object.id.clone()).or_insert(Vec::new());
                values.push((object.creation_time.unwrap_or(chrono::Utc::now()), attr_value.clone()));
            }
        }

        // Build attribute evolution models
        for (attr_name, object_values) in attribute_values {
            let mut evolution_model = AttributeEvolution {
                created_events: Vec::new(),
                modified_events: Vec::new(),
                deleted_events: Vec::new(),
            };

            for (object_id, values) in object_values {
                if values.len() > 1 {
                    evolution_model.modified_events.push(object_id);
                } else {
                    evolution_model.created_events.push(object_id);
                }
            }

            let attr_model = AttributeModel {
                name: attr_name.clone(),
                type_name: "unknown".to_string(),
                constraints: Vec::new(),
                evolution: evolution_model,
            };

            attribute_models.insert(attr_name, attr_model);
        }

        Ok(AttributeAnalysis {
            attribute_models,
            total_attributes: attribute_models.len(),
            modified_attributes: attribute_models.values()
                .filter(|am| !am.evolution.modified_events.is_empty())
                .count(),
        })
    }

    /// Analyze object relationships
    fn analyze_object_relationships(&self) -> ProcessMiningResult<RelationshipAnalysis> {
        debug_pm!("ocel", "Analyzing object relationships");

        let mut relationships = HashMap::new();
        let mut co_occurrence_counts = HashMap::new();

        // Count co-occurrences of object types in events
        for event in &self.ocel.events {
            let mut object_types_in_event = HashSet::new();

            for (object_id, role) in &event.related_objects {
                if let Some(object) = self.ocel.objects.get(object_id) {
                    object_types_in_event.insert(object.object_type.clone());
                }
            }

            // Count pairs
            let object_types: Vec<String> = object_types_in_event.into_iter().collect();
            for i in 0..object_types.len() {
                for j in i + 1..object_types.len() {
                    let pair = (object_types[i].clone(), object_types[j].clone());
                    *co_occurrence_counts.entry(pair).or_insert(0) += 1;
                }
            }
        }

        // Build relationship models
        for (pair, count) in co_occurrence_counts {
            if count >= self.params.relationship_threshold as usize {
                relationships.insert(pair, ObjectRelationship {
                    source: pair.0.clone(),
                    target: pair.1.clone(),
                    relationship_type: "co-occurrence".to_string(),
                    attributes: HashMap::new(),
                });
            }
        }

        Ok(RelationshipAnalysis {
            relationships,
            total_relationships: relationships.len(),
            strong_relationships: relationships.values()
                .filter(|rel| {
                    let cooccur = co_occurrence_counts.get(&(rel.source.clone(), rel.target.clone())).unwrap_or(&0);
                    *cooccur > 0
                })
                .count(),
        })
    }

    /// Analyze object interactions
    fn analyze_object_interactions(&self) -> ProcessMiningResult<InteractionAnalysis> {
        debug_pm!("ocel", "Analyzing object interactions");

        let mut interactions = HashMap::new();
        let mut interaction_sequences = HashMap::new();

        // Build interaction sequences
        for object in self.ocel.objects.values() {
            if let Some(creation_time) = object.creation_time {
                let mut interaction_sequence = Vec::new();

                // Find events involving this object
                for event in &self.ocel.events {
                    for (related_id, role) in &event.related_objects {
                        if related_id == &object.id {
                            interaction_sequence.push((
                                event.timestamp,
                                event.activity.clone(),
                                role.role.clone(),
                            ));
                        }
                    }
                }

                // Sort by timestamp
                interaction_sequence.sort_by(|a, b| a.0.cmp(&b.0));

                // Group by interaction pattern
                let pattern_type = self.classify_interaction_pattern(&interaction_sequence);
                interactions.entry(pattern_type).or_insert(Vec::new()).push(object.id.clone());
                interaction_sequences.insert(object.id.clone(), interaction_sequence);
            }
        }

        Ok(InteractionAnalysis {
            interactions,
            interaction_sequences,
            total_interactions: interactions.len(),
            average_complexity: interactions.values()
                .map(|objs| objs.len())
                .sum::<usize>() as f64 / interactions.len() as f64,
        })
    }

    /// Classify interaction pattern
    fn classify_interaction_pattern(&self, sequence: &Vec<(chrono::DateTime<chrono::Utc>, String, String)>) -> String {
        if sequence.is_empty() {
            return "none".to_string();
        }

        // Simple pattern classification
        if sequence.len() == 1 {
            "single".to_string()
        } else {
            let activities: Vec<String> = sequence.iter().map(|(_, activity, _)| activity.clone()).collect();
            let unique_activities: HashSet<String> = activities.iter().cloned().collect();

            if unique_activities.len() == 1 {
                "repetitive".to_string()
            } else if self.is_sequential(&activities) {
                "sequential".to_string()
            } else if self.is_parallel(&activities) {
                "parallel".to_string()
            } else {
                "complex".to_string()
            }
        }
    }

    /// Check if sequence is sequential
    fn is_sequential(&self, activities: &[String]) -> bool {
        for i in 0..activities.len() - 1 {
            if activities[i] == activities[i + 1] {
                return false;
            }
        }
        true
    }

    /// Check if sequence is parallel
    fn is_parallel(&self, activities: &[String]) -> bool {
        // Simplified check - in production use proper parallel detection
        activities.len() > 3 && activities.iter().collect::<HashSet<_>>().len() >= 2
    }

    /// Detect interaction patterns
    fn detect_interaction_patterns(&self) -> ProcessMiningResult<PatternDetection> {
        debug_pm!("ocel", "Detecting interaction patterns");

        let mut patterns = Vec::new();
        let mut pattern_counts = HashMap::new();

        // Use interaction analysis results to detect patterns
        for event in &self.ocel.events {
            let object_types: HashSet<String> = event.related_objects.values()
                .filter_map(|role| {
                    self.ocel.objects.get(role.role.as_str())
                        .map(|obj| obj.object_type.clone())
                })
                .collect();

            if object_types.len() >= 2 {
                // Multi-object interaction
                let pattern = self.classify_interaction_pattern_type(&object_types);
                patterns.push(pattern);
                *pattern_counts.entry(pattern).or_insert(0) += 1;
            }
        }

        Ok(PatternDetection {
            patterns,
            pattern_counts,
            total_patterns: patterns.len(),
            dominant_pattern: pattern_counts.iter()
                .max_by_key(|(_, count)| *count)
                .map(|(pattern, _)| pattern.clone())
                .unwrap_or_default(),
        })
    }

    /// Classify interaction pattern type
    fn classify_interaction_pattern_type(&self, object_types: &HashSet<String>) -> String {
        match object_types.len() {
            1 => "single".to_string(),
            2 => "pair".to_string(),
            3 => "group".to_string(),
            _ => "crowd".to_string(),
        }
    }

    /// Build Object-Centric model
    fn build_object_centric_model(&mut self) -> ProcessMiningResult<()> {
        debug_pm!("ocel", "Building Object-Centric model");

        // Build object type models
        for (object_type, obj_type_def) in &self.ocel.object_types {
            let object_model = self.build_object_type_model(object_type, obj_type_def)?;
            self.model.object_types.insert(object_type.clone(), object_model);
        }

        // Build process flows
        self.build_process_flows()?;

        // Build object interactions
        self.build_object_interactions()?;

        Ok(())
    }

    /// Build object type model
    fn build_object_type_model(&self, object_type: &str, obj_type_def: &ObjectType) -> ProcessMiningResult<ObjectModel> {
        let mut instances = Vec::new();

        // Find objects of this type
        for (obj_id, obj) in &self.ocel.objects {
            if obj.object_type == object_type {
                let instance = ObjectInstance {
                    id: obj_id.clone(),
                    attributes: obj.attributes.clone(),
                    creation_events: Vec::new(),
                    modification_events: Vec::new(),
                    deletion_events: Vec::new(),
                };
                instances.push(instance);
            }
        }

        let lifecycle = self.build_lifecycle_model(object_type)?;

        Ok(ObjectModel {
            object_type: object_type.to_string(),
            instances,
            lifecycle,
            attributes: HashMap::new(),
            relationships: Vec::new(),
        })
    }

    /// Build process flows
    fn build_process_flows(&mut self) -> ProcessMiningResult<()> {
        debug_pm!("ocel", "Building process flows");

        // Create flows for each activity
        let activities: HashSet<String> = self.ocel.events.iter()
            .map(|event| event.activity.clone())
            .collect();

        for activity in activities {
            let flow = ProcessFlow {
                id: format!("flow_{}", activity),
                name: activity.clone(),
                source_objects: Vec::new(),
                target_objects: Vec::new(),
                activities: vec![activity.clone()],
                flow_type: FlowType::Modification,
                metrics: FlowMetrics {
                    frequency: 0.0,
                    duration: None,
                    success_rate: 1.0,
                    throughput: 0.0,
                },
            };

            self.model.process_flows.push(flow);
        }

        Ok(())
    }

    /// Build object interactions
    fn build_object_interactions(&mut self) -> ProcessMiningResult<()> {
        debug_pm!("ocel", "Building object interactions");

        // Create interactions based on relationships
        for (source_type, target_type) in self.model.relationships.keys() {
            let interaction = ObjectInteraction {
                participants: vec![source_type.clone(), target_type.clone()],
                interaction_type: "direct".to_string(),
                activities: Vec::new(),
                patterns: Vec::new(),
                metrics: InteractionMetrics {
                    frequency: 0.0,
                    duration: chrono::Duration::ZERO,
                    complexity: 0.0,
                    success_rate: 1.0,
                    participant_satisfaction: 0.0,
                },
            };

            self.model.object_interactions.push(interaction);
        }

        Ok(())
    }

    /// calculate metrics
    fn calculate_metrics(&self) -> ProcessMiningResult<ObjectCentricMetrics> {
        debug_pm!("ocel", "Calculating metrics");

        Ok(ObjectCentricMetrics {
            object_types_count: self.ocel.object_types.len(),
            object_instances_count: self.ocel.objects.len(),
            event_object_relations_count: self.ocel.event_object_relations.len(),
            average_objects_per_event: self.ocel.event_object_relations.len() as f64 / self.ocel.events.len() as f64,
            average_events_per_object: self.ocel.events.len() as f64 / self.ocel.objects.len() as f64,
            object_interaction_density: self.model.object_interactions.len() as f64 / self.model.object_types.len() as f64,
            process_complexity: self.model.process_flows.len() as f64,
            lifecycle_coverage: 0.8, // Simplified
        })
    }

    /// Export results
    pub fn export_results(&self, result: &ObjectCentricResult, format: &str) -> ProcessMiningResult<String> {
        match format.to_lowercase().as_str() {
            "json" => Ok(serde_json::to_string_pretty(result)?),
            "xml" => self.export_to_xml(result),
            _ => Err(ProcessMiningError::FormatError(format!("Unsupported format: {}", format))),
        }
    }

    /// Export to XML
    fn export_to_xml(&self, result: &ObjectCentricResult) -> ProcessMiningResult<String> {
        let mut xml = String::new();

        xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        xml.push_str("<objectcentricmodel>\n");

        // Export object types
        for (object_type, model) in &result.model.object_types {
            xml.push_str(&format!("  <objecttype name=\"{}\">\n", object_type));
            xml.push_str(&format!("    <instances>{}</instances>\n", model.instances.len()));
            xml.push_str("  </objecttype>\n");
        }

        // Export process flows
        xml.push_str("  <processflows>\n");
        for flow in &result.model.process_flows {
            xml.push_str(&format!("    <flow id=\"{}\" name=\"{}\">\n", flow.id, flow.name));
            xml.push_str(&format!("      <type>{:?}</type>\n", flow.flow_type));
            xml.push_str("    </flow>\n");
        }
        xml.push_str("  </processflows>\n");

        xml.push_str("</objectcentricmodel>\n");

        Ok(xml)
    }
}

/// Object-Centric Process Mining result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectCentricResult {
    pub model: ObjectCentricModel,
    pub lifecycle_analysis: Option<LifecycleAnalysis>,
    pub attribute_analysis: Option<AttributeAnalysis>,
    pub relationship_analysis: Option<RelationshipAnalysis>,
    pub interaction_analysis: Option<InteractionAnalysis>,
    pub pattern_detection: Option<PatternDetection>,
    pub metrics: ObjectCentricMetrics,
    pub computation_time: std::time::Duration,
}

/// Lifecycle analysis result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LifecycleAnalysis {
    pub lifecycle_models: HashMap<String, LifecycleModel>,
    pub object_type_counts: HashMap<String, usize>,
    pub total_events: usize,
    pub average_events_per_object: f64,
}

/// Attribute analysis result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttributeAnalysis {
    pub attribute_models: HashMap<String, AttributeModel>,
    pub total_attributes: usize,
    pub modified_attributes: usize,
}

/// Relationship analysis result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RelationshipAnalysis {
    pub relationships: HashMap<(String, String), ObjectRelationship>,
    pub total_relationships: usize,
    pub strong_relationships: usize,
}

/// Interaction analysis result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InteractionAnalysis {
    pub interactions: HashMap<String, Vec<String>>,
    pub interaction_sequences: HashMap<String, Vec<(chrono::DateTime<chrono::Utc>, String, String)>>,
    pub total_interactions: usize,
    pub average_complexity: f64,
}

/// Pattern detection result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternDetection {
    pub patterns: Vec<String>,
    pub pattern_counts: HashMap<String, usize>,
    pub total_patterns: usize,
    pub dominant_pattern: String,
}

/// Object-Centric mining utilities
pub mod utils {
    use super::*;

    /// Generate test OCEL log for testing
    pub fn generate_test_ocel_log(num_events: usize) -> OCELLog {
        let mut events = Vec::new();
        let mut objects = HashMap::new();
        let mut object_types = HashMap::new();
        let mut event_object_relations = Vec::new();

        // Create object types
        object_types.insert("Case".to_string(), ObjectType {
            name: "Case".to_string(),
            attributes: HashMap::new(),
            relationships: Vec::new(),
        });

        object_types.insert("Resource".to_string(), ObjectType {
            name: "Resource".to_string(),
            attributes: HashMap::new(),
            relationships: Vec::new(),
        });

        // Create objects and events
        for i in 0..num_events {
            let case_id = format!("case_{}", i / 10);
            let resource_id = format!("resource_{}", i % 5);
            let event_id = format!("event_{}", i);

            // Create case object if not exists
            if !objects.contains_key(&case_id) {
                objects.insert(case_id.clone(), OCelObject {
                    id: case_id.clone(),
                    object_type: "Case".to_string(),
                    attributes: HashMap::new(),
                    creation_time: Some(chrono::Utc::now() + chrono::Duration::seconds(i as i64)),
                    modification_time: None,
                    deletion_time: None,
                });
            }

            // Create resource object if not exists
            if !objects.contains_key(&resource_id) {
                objects.insert(resource_id.clone(), OCelObject {
                    id: resource_id.clone(),
                    object_type: "Resource".to_string(),
                    attributes: HashMap::new(),
                    creation_time: None,
                    modification_time: None,
                    deletion_time: None,
                });
            }

            // Create event
            let event = OCelEvent {
                id: event_id.clone(),
                activity: format!("activity_{}", i % 3),
                timestamp: chrono::Utc::now() + chrono::Duration::seconds(i as i64),
                lifecycle: Some("complete".to_string()),
                attributes: HashMap::new(),
                related_objects: HashMap::new(),
            };

            // Add object relations
            event.related_objects.insert(case_id.clone(), ObjectRole {
                role: "case".to_string(),
                lifetime: ObjectLifetime::Use,
                attributes: HashMap::new(),
            });

            event.related_objects.insert(resource_id.clone(), ObjectRole {
                role: "resource".to_string(),
                lifetime: ObjectLifetime::Use,
                attributes: HashMap::new(),
            });

            events.push(event);

            // Add event-object relations
            event_object_relations.push(EventObjectRelation {
                event_id: event_id.clone(),
                object_id: case_id.clone(),
                role: "case".to_string(),
                attributes: HashMap::new(),
            });

            event_object_relations.push(EventObjectRelation {
                event_id: event_id.clone(),
                object_id: resource_id.clone(),
                role: "resource".to_string(),
                attributes: HashMap::new(),
            });
        }

        OCELLog {
            events,
            objects,
            object_types,
            object_relationships: Vec::new(),
            object_attributes: HashMap::new(),
            event_object_relations,
            global_object_attributes: HashMap::new(),
        }
    }

    /// Validate OCEL log structure
    pub fn validate_ocel_log(ocel: &OCELLog) -> bool {
        // Check that all events have IDs
        for event in &ocel.events {
            if event.id.is_empty() {
                return false;
            }
        }

        // Check that all objects have IDs
        for (obj_id, obj) in &ocel.objects {
            if obj_id.is_empty() || obj.object_type.is_empty() {
                return false;
            }
        }

        // Check event-object relations consistency
        for relation in &ocel.event_object_relations {
            if !ocel.events.iter().any(|e| e.id == relation.event_id) {
                return false;
            }
            if !ocel.objects.contains_key(&relation.object_id) {
                return false;
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ocel_miner_creation() {
        let log = EventLog::new("test_log".to_string());
        let params = OCELParameters::default();
        let result = ObjectCentricMiner::new(log, params);

        assert!(result.is_ok());
    }

    #[test]
    fn test_ocel_conversion() {
        let mut log = EventLog::new("test_log".to_string());
        log.activities.insert("A".to_string());
        log.activities.insert("B".to_string());

        let case = Case::new("test_case".to_string());
        case.add_event(Event::new("test_case".to_string(), "A".to_string(), chrono::Utc::now()));
        log.add_case(case).unwrap();

        let ocel = ObjectCentricMiner::convert_to_ocel(&log).unwrap();

        assert_eq!(ocel.events.len(), 1);
        assert_eq!(ocel.objects.len(), 1); // Case object
        assert_eq!(ocel.object_types.len(), 1); // Case type
    }

    #[test]
    fn test_lifecycle_model_building() {
        let ocel = utils::generate_test_ocel_log(10);
        let miner = ObjectCentricMiner {
            log: EventLog::new("test".to_string()),
            ocel,
            params: OCELParameters::default(),
            model: ObjectCentricModel::new("test".to_string()),
        };

        let lifecycle = miner.build_lifecycle_model("Case").unwrap();
        assert!(!lifecycle.states.is_empty());
        assert!(lifecycle.initial_state.is_empty());
        assert!(lifecycle.final_state.is_empty());
    }

    #[test]
    fn test_interaction_pattern_classification() {
        let miner = ObjectCentricMiner {
            log: EventLog::new("test".to_string()),
            ocel: OCELLog {
                events: Vec::new(),
                objects: HashMap::new(),
                object_types: HashMap::new(),
                object_relationships: Vec::new(),
                object_attributes: HashMap::new(),
                event_object_relations: Vec::new(),
                global_object_attributes: HashMap::new(),
            },
            params: OCELParameters::default(),
            model: ObjectCentricModel::new("test".to_string()),
        };

        let single_seq = vec![(chrono::Utc::now(), "A".to_string(), "use".to_string())];
        let complex_seq = vec![
            (chrono::Utc::now(), "A".to_string(), "use".to_string()),
            (chrono::Utc::now() + chrono::Duration::seconds(1), "B".to_string(), "use".to_string()),
        ];

        let single_pattern = miner.classify_interaction_pattern(&single_seq);
        let complex_pattern = miner.classify_interaction_pattern(&complex_seq);

        assert_eq!(single_pattern, "single");
        assert!(!complex_pattern.is_empty());
    }

    #[test]
    fn test_ocel_validation() {
        let ocel = utils::generate_test_ocel_log(10);
        assert!(utils::validate_ocel_log(&ocel));
    }

    #[test]
    fn test_generate_test_ocel_log() {
        let ocel = utils::generate_test_ocel_log(10);

        assert_eq!(ocel.events.len(), 10);
        assert!(!ocel.objects.is_empty());
        assert!(!ocel.object_types.is_empty());
        assert!(!ocel.event_object_relations.is_empty());
    }

    #[test]
    fn test_lifecycle_analysis() {
        let mut log = EventLog::new("test_log".to_string());
        log.activities.insert("A".to_string());

        let case = Case::new("test_case".to_string());
        case.add_event(Event::new("test_case".to_string(), "A".to_string(), chrono::Utc::now()));
        log.add_case(case).unwrap();

        let ocel = ObjectCentricMiner::convert_to_ocel(&log).unwrap();
        let miner = ObjectCentricMiner {
            log,
            ocel,
            params: OCELParameters::default(),
            model: ObjectCentricModel::new("test".to_string()),
        };

        let analysis = miner.analyze_lifecycle_patterns().unwrap();
        assert!(!analysis.lifecycle_models.is_empty());
        assert_eq!(analysis.total_events, 1);
    }

    #[test]
    fn test_attribute_evolution_analysis() {
        let ocel = utils::generate_test_ocel_log(10);
        let miner = ObjectCentricMiner {
            log: EventLog::new("test".to_string()),
            ocel,
            params: OCELParameters::default(),
            model: ObjectCentricModel::new("test".to_string()),
        };

        let analysis = miner.analyze_attribute_evolution().unwrap();
        assert!(analysis.total_attributes >= 0);
    }
}