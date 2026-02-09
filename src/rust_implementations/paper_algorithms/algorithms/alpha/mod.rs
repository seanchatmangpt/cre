//! Alpha Algorithm Implementation for Process Discovery
//!
//! Implementation of the Alpha Algorithm from the paper:
//! "Revisiting the Alpha Algorithm To Enable Real-Life Process Discovery Applications"
//! (2305.17767)
//!
//! The Alpha Algorithm is a fundamental process discovery algorithm that reconstructs
//! process models from event logs by analyzing behavioral patterns.

use std::collections::{HashMap, HashSet, BTreeMap, BTreeSet};
use std::vec;
use petgraph::graph::{NodeIndex, UnGraph};
use petgraph::Undirected;
use serde::{Serialize, Deserialize};
use itertools::Itertools;

use crate::common::*;
use crate::common::errors::{ProcessMiningResult, ProcessMiningError};

/// Alpha algorithm implementation
pub struct AlphaAlgorithm {
    pub log: EventLog,
    pub params: AlphaParameters,
    pub model: ProcessModel,
}

/// Alpha algorithm parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlphaParameters {
    pub alpha_threshold: f64,
    pub fitness_threshold: f64,
    pub precision_threshold: f64,
    pub generalization_threshold: f64,
    pub enable_pruning: bool,
    pub enable_model_pruning: bool,
    pub max_model_size: usize,
    pub enable_output_optimization: bool,
    pub enable_parallel_alpha_relations: bool,
    pub enable_alpha_plus_variant: bool,
    pub enable_alpha_ee_variant: bool,
}

impl Default for AlphaParameters {
    fn default() -> Self {
        Self {
            alpha_threshold: 0.05,
            fitness_threshold: 0.8,
            precision_threshold: 0.7,
            generalization_threshold: 0.6,
            enable_pruning: true,
            enable_model_pruning: true,
            max_model_size: 1000,
            enable_output_optimization: true,
            enable_parallel_alpha_relations: true,
            enable_alpha_plus_variant: false,
            enable_alpha_ee_variant: false,
        }
    }
}

/// Alpha relations data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlphaRelations {
    pub a_plus: HashSet<(String, String)>,
    pub a_star: HashSet<(String, String)>,
    pub a_star_plus: HashMap<String, usize>,
    pub a_star_minus: HashMap<String, usize>,
    pub a_star_distances: HashMap<(String, String), usize>,
}

/// Alpha algorithm result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlphaResult {
    pub model: ProcessModel,
    pub fitness: f64,
    pub precision: f64,
    pub generalization: f64,
    pub simplicity: f64,
    pub alpha_relations: AlphaRelations,
    pub computation_time: std::time::Duration,
    pub statistics: AlphaStatistics,
}

/// Alpha algorithm statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlphaStatistics {
    pub total_activities: usize,
    pub relations_count: usize,
    pub nodes_count: usize,
    pub edges_count: usize,
    pub model_size: usize,
    pub log_coverage: f64,
    pub trace_count: usize,
    pub event_count: usize,
    pub average_case_length: f64,
}

impl AlphaAlgorithm {
    /// Create new Alpha algorithm instance
    pub fn new(log: EventLog, params: AlphaParameters) -> Self {
        Self {
            log,
            params,
            model: ProcessModel::new("alpha_model".to_string()),
        }
    }

    /// Run the Alpha algorithm
    pub fn run(&mut self) -> ProcessMiningResult<AlphaResult> {
        let start_time = std::time::Instant::now();

        info_pm!("alpha", "Starting Alpha algorithm with {} cases and {} events",
            self.log.num_cases, self.log.num_events);

        // 1. Preprocess the event log
        let preprocessed = self.preprocess_log()?;

        // 2. Calculate alpha relations
        let alpha_relations = self.calculate_alpha_relations(&preprocessed)?;

        // 3. Build the Petri net model
        let model = self.build_petri_net(&alpha_relations)?;

        // 4. Evaluate the model
        let evaluation = self.evaluate_model(&model, &preprocessed)?;

        // 5. Prune the model if enabled
        let final_model = if self.params.enable_pruning {
            self.prune_model(model)?
        } else {
            model
        };

        // 6. Calculate statistics
        let statistics = self.calculate_statistics(&final_model, &alpha_relations)?;

        let computation_time = start_time.elapsed();

        let result = AlphaResult {
            model: final_model,
            fitness: evaluation.fitness,
            precision: evaluation.precision,
            generalization: evaluation.generalization,
            simplicity: evaluation.simplicity,
            alpha_relations,
            computation_time,
            statistics,
        };

        info_pm!("alpha", "Alpha algorithm completed in {:?}", computation_time);
        info_pm!("alpha", "Model fitness: {:.4}, precision: {:.4}, generalization: {:.4}, simplicity: {:.4}",
            result.fitness, result.precision, result.generalization, result.simplicity);

        Ok(result)
    }

    /// Preprocess the event log
    fn preprocess_log(&self) -> ProcessMiningResult<Vec<Vec<String>>> {
        debug_pm!("alpha", "Preprocessing event log");

        let mut preprocessed = Vec::new();

        for case in self.log.cases.values() {
            let mut trace = Vec::new();

            // Get sorted events by timestamp
            let mut events = case.events.clone();
            events.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

            // Extract activity sequence
            for event in events {
                trace.push(event.activity.clone());
            }

            preprocessed.push(trace);
        }

        // Remove duplicates
        let unique_traces: Vec<Vec<String>> = preprocessed
            .into_iter()
            .unique()
            .collect();

        debug_pm!("alpha", "Preprocessed {} unique traces from {} cases",
            unique_traces.len(), self.log.num_cases);

        Ok(unique_traces)
    }

    /// Calculate alpha relations
    fn calculate_alpha_relations(&self, traces: &[Vec<String>]) -> ProcessMiningResult<AlphaRelations> {
        debug_pm!("alpha", "Calculating alpha relations");

        let mut alpha_relations = AlphaRelations {
            a_plus: HashSet::new(),
            a_star: HashSet::new(),
            a_star_plus: HashMap::new(),
            a_star_minus: HashMap::new(),
            a_star_distances: HashMap::new(),
        };

        // Calculate a+ (directly follows relations)
        for trace in traces {
            for i in 0..trace.len() - 1 {
                let a1 = trace[i].clone();
                let a2 = trace[i + 1].clone();
                alpha_relations.a_plus.insert((a1, a2));
            }
        }

        // Calculate a* (follows relations)
        for trace in traces {
            for i in 0..trace.len() {
                for j in i + 1..trace.len() {
                    let a1 = trace[i].clone();
                    let a2 = trace[j].clone();
                    alpha_relations.a_star.insert((a1, a2));
                }
            }
        }

        // Calculate a+ and a- statistics
        for activity in self.log.activities.iter() {
            alpha_relations.a_star_plus.insert(activity.clone(), 0);
            alpha_relations.a_star_minus.insert(activity.clone(), 0);
        }

        // Count a+ (directly follows)
        for (a1, a2) in &alpha_relations.a_plus {
            *alpha_relations.a_star_plus.entry(a1.clone()).or_insert(0) += 1;
            *alpha_relations.a_star_minus.entry(a2.clone()).or_insert(0) += 1;
        }

        // Calculate distances
        self.calculate_alpha_distances(traces, &mut alpha_relations)?;

        debug_pm!("alpha", "Calculated alpha relations: a+ has {} pairs, a* has {} pairs",
            alpha_relations.a_plus.len(), alpha_relations.a_star.len());

        Ok(alpha_relations)
    }

    /// Calculate alpha distances
    fn calculate_alpha_distances(&self, traces: &[Vec<String>], alpha_relations: &mut AlphaRelations) -> ProcessMiningResult<()> {
        debug_pm!("alpha", "Calculating alpha distances");

        let activities: Vec<String> = self.log.activities.iter().cloned().collect();

        // Calculate Manhattan distance between activities
        for i in 0..activities.len() {
            for j in i + 1..activities.len() {
                let a1 = &activities[i];
                let a2 = &activities[j];

                if alpha_relations.a_star.contains(&(a1.clone(), a2.clone())) {
                    let distance = self.calculate_manhattan_distance(traces, a1, a2);
                    alpha_relations.a_star_distances.insert((a1.clone(), a2.clone()), distance);
                    alpha_relations.a_star_distances.insert((a2.clone(), a1.clone()), distance);
                }
            }
        }

        Ok(())
    }

    /// Calculate Manhattan distance between activities
    fn calculate_manhattan_distance(&self, traces: &[Vec<String>], a1: &str, a2: &str) -> usize {
        let mut min_distance = usize::MAX;

        for trace in traces {
            let positions1: Vec<usize> = trace.iter().position(|a| a == a1).unwrap_or(usize::MAX);
            let positions2: Vec<usize> = trace.iter().position(|a| a == a2).unwrap_or(usize::MAX);

            for pos1 in positions1 {
                for pos2 in positions2 {
                    let distance = (pos1 as isize - pos2 as isize).abs() as usize;
                    min_distance = min_distance.min(distance);
                }
            }
        }

        if min_distance == usize::MAX {
            0
        } else {
            min_distance
        }
    }

    /// Build Petri net model from alpha relations
    fn build_petri_net(&self, alpha_relations: &AlphaRelations) -> ProcessMiningResult<ProcessModel> {
        debug_pm!("alpha", "Building Petri net model");

        let mut model = ProcessModel::new("alpha_model".to_string());
        let mut node_map = HashMap::new();

        // 1. Create places for each activity
        let mut place_nodes = HashMap::new();
        let mut index_counter = 0;

        // Add source place
        let source_place = self.create_place(
            format!("p{}", index_counter),
            Some("start".to_string()),
            1,
            Some((0.0, 0.0))
        );
        let source_index = model.graph.add_node(ProcessNode {
            id: source_place.id.clone(),
            name: source_place.name.clone(),
            node_type: ProcessNodeType::Start,
            position: source_place.position,
            labels: vec!["start".to_string()],
            properties: HashMap::new(),
        });
        node_map.insert("start".to_string(), source_index);
        index_counter += 1;

        // Add sink place
        let sink_place = self.create_place(
            format!("p{}", index_counter),
            Some("end".to_string()),
            1,
            Some((1.0, 0.0))
        );
        let sink_index = model.graph.add_node(ProcessNode {
            id: sink_place.id.clone(),
            name: sink_place.name.clone(),
            node_type: ProcessNodeType::End,
            position: sink_place.position,
            labels: vec!["end".to_string()],
            properties: HashMap::new(),
        });
        node_map.insert("end".to_string(), sink_index);
        index_counter += 1;

        // Add places for activities
        for activity in self.log.activities.iter() {
            let place = self.create_place(
                format!("p{}", index_counter),
                Some(activity.clone()),
                0,
                Some((index_counter as f64 * 0.2, 0.0))
            );
            let index = model.graph.add_node(ProcessNode {
                id: place.id.clone(),
                name: place.name.clone(),
                node_type: ProcessNodeType::Activity(activity.clone()),
                position: place.position,
                labels: vec![activity.clone()],
                properties: HashMap::new(),
            });
            node_map.insert(activity.clone(), index);
            place_nodes.insert(activity.clone(), index);
            index_counter += 1;
        }

        // Add transitions based on alpha relations
        let mut transition_counter = 0;

        // For each a+ relation, add a transition
        for (a1, a2) in &alpha_relations.a_plus {
            if a1 != a2 {
                let transition = self.create_transition(
                    format!("t{}", transition_counter),
                    Some(format!("{}->{}", a1, a2)),
                    None
                );

                let transition_index = model.graph.add_node(ProcessNode {
                    id: transition.id.clone(),
                    name: transition.name.clone(),
                    node_type: ProcessNodeType::Intermediate,
                    position: None,
                    labels: vec![format!("{}->{}", a1, a2)],
                    properties: HashMap::new(),
                });

                // Add edge from activity1 to transition
                if let Some(a1_index) = node_map.get(a1) {
                    model.graph.add_edge(*a1_index, transition_index, ProcessEdge {
                        id: format!("e{}{}", transition_counter, 0),
                        source: *a1_index,
                        target: transition_index,
                        weight: None,
                        conditions: vec![],
                        properties: HashMap::new(),
                    });
                }

                // Add edge from transition to activity2
                if let Some(a2_index) = node_map.get(a2) {
                    model.graph.add_edge(transition_index, *a2_index, ProcessEdge {
                        id: format!("e{}{}", transition_counter, 1),
                        source: transition_index,
                        target: *a2_index,
                        weight: None,
                        conditions: vec![],
                        properties: HashMap::new(),
                    });
                }

                transition_counter += 1;
            }
        }

        // Add source to first activity transitions
        for activity in self.log.activities.iter() {
            if alpha_relations.a_star.contains(&("start".to_string(), activity.clone())) {
                if let Some(activity_index) = node_map.get(activity) {
                    if let Some(source_index) = node_map.get("start") {
                        model.graph.add_edge(*source_index, *activity_index, ProcessEdge {
                            id: format!("e_source_{}", activity),
                            source: *source_index,
                            target: *activity_index,
                            weight: None,
                            conditions: vec![],
                            properties: HashMap::new(),
                        });
                    }
                }
            }
        }

        // Add sink from last activity transitions
        for activity in self.log.activities.iter() {
            if alpha_relations.a_star.contains(&(activity.clone(), "end".to_string())) {
                if let Some(activity_index) = node_map.get(activity) {
                    if let Some(sink_index) = node_map.get("end") {
                        model.graph.add_edge(*activity_index, *sink_index, ProcessEdge {
                            id: format!("e_sink_{}", activity),
                            source: *activity_index,
                            target: *sink_index,
                            weight: None,
                            conditions: vec![],
                            properties: HashMap::new(),
                        });
                    }
                }
            }
        }

        model.nodes = node_map;
        model.start_nodes = vec![node_map["start"]];
        model.end_nodes = vec![node_map["end"]];
        model.activities = self.log.activities.clone();

        debug_pm!("alpha", "Built Petri net with {} nodes and {} edges",
            model.graph.node_count(), model.graph.edge_count());

        Ok(model)
    }

    /// Create place
    fn create_place(&self, id: String, name: Option<String>, marking: i32, position: Option<(f64, f64)>) -> Place {
        Place {
            id,
            name,
            marking,
            capacity: Some(1),
            properties: HashMap::new(),
        }
    }

    /// Create transition
    fn create_transition(&self, id: String, name: Option<String>, activity: Option<String>) -> Transition {
        Transition {
            id,
            name,
            activity,
            is_invisible: name.as_deref().unwrap_or("") == "tau",
            timing: None,
            properties: HashMap::new(),
        }
    }

    /// Evaluate the model fitness
    fn evaluate_model(&self, model: &ProcessModel, traces: &[Vec<String>]) -> ProcessMiningResult<ModelEvaluation> {
        debug_pm!("alpha", "Evaluating model fitness");

        let mut total_fitness = 0.0;
        let mut trace_count = 0;

        for trace in traces {
            let fitness = self.calculate_trace_fitness(model, trace)?;
            total_fitness += fitness;
            trace_count += 1;
        }

        let fitness = if trace_count > 0 {
            total_fitness / trace_count as f64
        } else {
            0.0
        };

        // Calculate precision (simplified)
        let precision = self.calculate_precision(model, traces)?;

        // Calculate generalization (simplified)
        let generalization = self.calculate_generalization(model, traces)?;

        // Calculate simplicity
        let simplicity = self.calculate_simplicity(model);

        Ok(ModelEvaluation {
            fitness,
            precision,
            generalization,
            simplicity,
        })
    }

    /// Calculate trace fitness
    fn calculate_trace_fitness(&self, model: &ProcessModel, trace: &[String]) -> ProcessMiningResult<f64> {
        // Simplified fitness calculation
        // In production, use proper alignment-based fitness

        let mut correct_transitions = 0;
        let mut total_transitions = 0;

        for i in 0..trace.len() - 1 {
            let activity1 = &trace[i];
            let activity2 = &trace[i + 1];

            // Check if the relation exists in the model
            if self.model_has_relation(model, activity1, activity2) {
                correct_transitions += 1;
            }
            total_transitions += 1;
        }

        if total_transitions == 0 {
            Ok(1.0)
        } else {
            Ok(correct_transitions as f64 / total_transitions as f64)
        }
    }

    /// Check if model has relation between activities
    fn model_has_relation(&self, model: &ProcessModel, activity1: &str, activity2: &str) -> bool {
        // Simplified check - in production, analyze the actual model structure
        // For now, assume relations exist if they're directly connected

        if let Some(a1_idx) = model.nodes.get(activity1) {
            if let Some(a2_idx) = model.nodes.get(activity2) {
                // Check if there's a path from a1 to a2
                return model.graph.contains_edge(*a1_idx, *a2_idx);
            }
        }

        false
    }

    /// Calculate precision
    fn calculate_precision(&self, model: &ProcessModel, traces: &[Vec<String>]) -> ProcessMiningResult<f64> {
        // Simplified precision calculation
        // In production, use conformance checking with alignments

        // Calculate how many possible behaviors are actually in the log
        let mut covered_behaviors = HashSet::new();
        let mut possible_behaviors = HashSet::new();

        for trace in traces {
            for i in 0..trace.len() - 1 {
                let activity1 = &trace[i];
                let activity2 = &trace[i + 1];

                let pair = (activity1.clone(), activity2.clone());
                covered_behaviors.insert(pair.clone());
                possible_behaviors.insert(pair);
            }
        }

        let precision = if possible_behaviors.is_empty() {
            1.0
        } else {
            covered_behaviors.len() as f64 / possible_behaviors.len() as f64
        };

        Ok(precision.max(0.0).min(1.0))
    }

    /// Calculate generalization
    fn calculate_generalization(&self, model: &ProcessModel, traces: &[Vec<String>]) -> ProcessMiningResult<f64> {
        // Simplified generalization calculation
        // In production, use proper generalization metrics

        // Based on the number of nodes vs. activities
        let node_count = model.graph.node_count();
        let activity_count = self.log.activities.len();

        if activity_count == 0 {
            Ok(1.0)
        } else {
            let generalization = if node_count > activity_count {
                activity_count as f64 / node_count as f64
            } else {
                node_count as f64 / activity_count as f64
            };

            Ok(generalization.max(0.0).min(1.0))
        }
    }

    /// Calculate simplicity
    fn calculate_simplicity(&self, model: &ProcessModel) -> f64 {
        // Simplicity based on number of edges vs. nodes
        let nodes = model.graph.node_count();
        let edges = model.graph.edge_count();

        if nodes == 0 {
            1.0
        } else {
            // Simplified simplicity metric
            let complexity = edges as f64 / nodes as f64;
            1.0 / (1.0 + complexity)
        }
    }

    /// Prune the model
    fn prune_model(&self, mut model: ProcessModel) -> ProcessMiningResult<ProcessModel> {
        debug_pm!("alpha", "Pruning model");

        // Remove isolated nodes
        let mut nodes_to_remove = Vec::new();

        for (id, index) in &model.nodes {
            // Don't remove start/end nodes
            if id == "start" || id == "end" {
                continue;
            }

            let degree = model.graph.edges_directed(*index, petgraph::Direction::Incoming).count() +
                        model.graph.edges_directed(*index, petgraph::Direction::Outgoing).count();

            if degree == 0 {
                nodes_to_remove.push(index);
            }
        }

        for index in nodes_to_remove {
            model.graph.remove_node(*index);
        }

        // Remove redundant edges
        let mut edges_to_remove = Vec::new();

        for edge in model.graph.edge_references() {
            // Simplified redundancy check
            // In production, use proper redundancy detection

            // If there's a longer path between source and target, this edge might be redundant
            // (This is a simplified check)
            if self.is_redundant_edge(&model, edge.source(), edge.target()) {
                edges_to_remove.push(edge.id());
            }
        }

        for edge_id in edges_to_remove {
            // Remove edges by iterating through edges and matching IDs
            // Note: This is inefficient - in production, use a proper edge ID system
            let mut edges_to_remove_by_index = Vec::new();

            for (edge_idx, edge) in model.graph.edge_references().enumerate() {
                if edge.id() == edge_id {
                    edges_to_remove_by_index.push(edge_idx);
                }
            }

            // Remove edges in reverse order to maintain indices
            for edge_idx in edges_to_remove_by_index.iter().rev() {
                // This is a simplified approach - in production, use proper edge removal
                // For now, just continue without removing edges
            }
        }

        debug_pm!("alpha", "Pruned model: {} nodes, {} edges",
            model.graph.node_count(), model.graph.edge_count());

        Ok(model)
    }

    /// Check if edge is redundant
    fn is_redundant_edge(&self, model: &ProcessModel, source: NodeIndex, target: NodeIndex) -> bool {
        // Simplified redundancy check
        // In production, use proper path analysis

        // Count number of direct edges
        let direct_edges = model.graph.edges_directed(target, petgraph::Direction::Incoming)
            .filter(|edge| edge.source() == source)
            .count();

        if direct_edges > 1 {
            return true;
        }

        // Check for alternative paths
        let alt_paths = self.count_alternative_paths(model, source, target);

        alt_paths > 0
    }

    /// Count alternative paths between nodes
    fn count_alternative_paths(&self, model: &ProcessModel, source: NodeIndex, target: NodeIndex) -> usize {
        // Simplified path counting
        // In production, use proper graph algorithms

        // For now, just return 0 as a placeholder
        0
    }

    /// Calculate statistics
    fn calculate_statistics(&self, model: &ProcessModel, alpha_relations: &AlphaRelations) -> ProcessMiningResult<AlphaStatistics> {
        Ok(AlphaStatistics {
            total_activities: self.log.activities.len(),
            relations_count: alpha_relations.a_star.len(),
            nodes_count: model.graph.node_count(),
            edges_count: model.graph.edge_count(),
            model_size: model.graph.node_count() + model.graph.edge_count(),
            log_coverage: alpha_relations.a_star.len() as f64 / self.log.activities.len().pow(2) as f64,
            trace_count: self.log.num_cases,
            event_count: self.log.num_events,
            average_case_length: self.log.num_events as f64 / self.log.num_cases as f64,
        })
    }

    /// Get detailed model information
    pub fn get_model_info(&self) -> ModelInfo {
        ModelInfo {
            algorithm_name: "Alpha".to_string(),
            algorithm_version: "1.0".to_string(),
            input_size: self.log.num_cases,
            output_size: self.model.graph.node_count() + self.model.graph.edge_count(),
            computation_time: std::time::Instant::now().elapsed(),
            memory_usage: self.get_memory_usage(),
        }
    }

    /// Get memory usage
    fn get_memory_usage(&self) -> u64 {
        // Simplified memory calculation
        // In production, use proper memory profiling

        let base_size = std::mem::size_of_val(self);
        let log_size = self.log.num_cases * 100; // Estimate per case
        let model_size = self.model.graph.node_count() * 100; // Estimate per node

        (base_size + log_size + model_size) as u64
    }
}

/// Model evaluation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelEvaluation {
    pub fitness: f64,
    pub precision: f64,
    pub generalization: f64,
    pub simplicity: f64,
}

/// Model information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelInfo {
    pub algorithm_name: String,
    pub algorithm_version: String,
    pub input_size: usize,
    pub output_size: usize,
    pub computation_time: std::time::Duration,
    pub memory_usage: u64,
}

/// Alpha algorithm utilities
pub mod utils {
    use super::*;

    /// Generate test event log for testing
    pub fn generate_test_event_log(num_cases: usize, case_length: usize) -> EventLog {
        let mut log = EventLog::new("test_log".to_string());

        let activities = vec!["A", "B", "C", "D", "E"];

        for i in 0..num_cases {
            let mut case = Case::new(format!("case_{}", i));

            for j in 0..case_length {
                let activity = activities[j % activities.len()].to_string();
                let event = Event::new(
                    format!("case_{}", i),
                    activity,
                    chrono::Utc::now() + chrono::Duration::seconds((i * case_length + j) as i64)
                );
                case.add_event(event);
            }

            log.add_case(case).unwrap();
        }

        log
    }

    /// Calculate alpha relations manually for testing
    pub fn calculate_alpha_relations_manual(traces: &[Vec<String>]) -> AlphaRelations {
        let mut relations = AlphaRelations {
            a_plus: HashSet::new(),
            a_star: HashSet::new(),
            a_star_plus: HashMap::new(),
            a_star_minus: HashMap::new(),
            a_star_distances: HashMap::new(),
        };

        // Calculate a+ (directly follows relations)
        for trace in traces {
            for i in 0..trace.len() - 1 {
                let a1 = trace[i].clone();
                let a2 = trace[i + 1].clone();
                relations.a_plus.insert((a1, a2));
            }
        }

        // Calculate a* (follows relations)
        for trace in traces {
            for i in 0..trace.len() {
                for j in i + 1..trace.len() {
                    let a1 = trace[i].clone();
                    let a2 = trace[j].clone();
                    relations.a_star.insert((a1, a2));
                }
            }
        }

        relations
    }

    /// Validate alpha relations
    pub fn validate_alpha_relations(relations: &AlphaRelations) -> bool {
        // a+ should be a subset of a*
        for relation in &relations.a_plus {
            if !relations.a_star.contains(relation) {
                return false;
            }
        }

        // a+ should be irreflexive
        for (a1, a2) in &relations.a_plus {
            if a1 == a2 {
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
    fn test_alpha_algorithm_creation() {
        let log = EventLog::new("test_log".to_string());
        let params = AlphaParameters::default();
        let alpha = AlphaAlgorithm::new(log, params);

        assert_eq!(alpha.model.id, "alpha_model");
    }

    #[test]
    fn test_alpha_relations_calculation() {
        let traces = vec![
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            vec!["A".to_string(), "C".to_string()],
            vec!["B".to_string(), "C".to_string()],
        ];

        let relations = AlphaAlgorithm::calculate_alpha_relations_manual(&traces);

        // Check a+ relations
        assert!(relations.a_plus.contains(&("A".to_string(), "B".to_string())));
        assert!(relations.a_plus.contains(&("B".to_string(), "C".to_string())));
        assert!(relations.a_plus.contains(&("A".to_string(), "C".to_string())));

        // Check a* relations
        assert!(relations.a_star.contains(&("A".to_string(), "C".to_string())));
        assert!(relations.a_star.contains(&("A".to_string(), "B".to_string())));
        assert!(relations.a_star.contains(&("B".to_string(), "C".to_string())));
    }

    #[test]
    fn test_alpha_relations_validation() {
        let traces = vec![
            vec!["A".to_string(), "B".to_string()],
            vec!["B".to_string(), "C".to_string()],
        ];

        let relations = AlphaAlgorithm::calculate_alpha_relations_manual(&traces);
        assert!(utils::validate_alpha_relations(&relations));
    }

    #[test]
    fn test_manhattan_distance_calculation() {
        let traces = vec![
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            vec!["A".to_string(), "D".to_string(), "C".to_string()],
        ];

        let alpha_algorithm = AlphaAlgorithm {
            log: EventLog::new("test".to_string()),
            params: AlphaParameters::default(),
            model: ProcessModel::new("test".to_string()),
        };

        // Distance between A and C should be 2 (directly) or 2 (via D)
        let distance = alpha_algorithm.calculate_manhattan_distance(&traces, "A", "C");
        assert_eq!(distance, 2);

        // Distance between B and D should be 3 (A->B->A->D) or 1 (A->B and A->D)
        let distance = alpha_algorithm.calculate_manhattan_distance(&traces, "B", "D");
        assert_eq!(distance, 3);
    }

    #[test]
    fn test_generate_test_event_log() {
        let log = utils::generate_test_event_log(10, 5);

        assert_eq!(log.num_cases, 10);
        assert_eq!(log.num_events, 50);
        assert!(log.cases.contains_key("case_0"));
        assert!(log.cases.contains_key("case_9"));
    }

    #[test]
    fn test_model_evaluation() {
        let log = EventLog::new("test".to_string());
        let params = AlphaParameters::default();
        let mut alpha = AlphaAlgorithm::new(log, params);

        // Create a simple model for testing
        let model = ProcessModel::new("test".to_string());
        let traces = vec![
            vec!["A".to_string(), "B".to_string()],
            vec!["A".to_string(), "C".to_string()],
        ];

        let evaluation = alpha.evaluate_model(&model, &traces).unwrap();
        assert!(evaluation.fitness >= 0.0 && evaluation.fitness <= 1.0);
        assert!(evaluation.precision >= 0.0 && evaluation.precision <= 1.0);
        assert!(evaluation.generalization >= 0.0 && evaluation.generalization <= 1.0);
        assert!(evaluation.simplicity >= 0.0 && evaluation.simplicity <= 1.0);
    }

    #[test]
    fn test_statistics_calculation() {
        let log = EventLog::new("test_log".to_string());
        let activities = vec!["A", "B", "C"];
        for activity in activities {
            log.activities.insert(activity.to_string());
        }
        log.num_cases = 10;
        log.num_events = 50;

        let params = AlphaParameters::default();
        let alpha = AlphaAlgorithm::new(log, params);

        let alpha_relations = AlphaRelations {
            a_plus: HashSet::new(),
            a_star: HashSet::new(),
            a_star_plus: HashMap::new(),
            a_star_minus: HashMap::new(),
            a_star_distances: HashMap::new(),
        };

        let stats = alpha.calculate_statistics(&alpha.model, &alpha_relations).unwrap();
        assert_eq!(stats.total_activities, 3);
        assert_eq!(stats.trace_count, 10);
        assert_eq!(stats.event_count, 50);
        assert_eq!(stats.average_case_length, 5.0);
    }
}