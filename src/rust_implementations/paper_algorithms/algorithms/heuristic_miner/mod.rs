//! Heuristic Miner Implementation for Process Discovery
//!
//! Implementation of the Heuristic Miner from the paper:
//! "Heuristics Miners for Streaming Event Data" (1212.6383)
//!
//! The Heuristic Miner is a noise-tolerant process discovery algorithm that
//! uses dependency analysis and heuristics to build process models.

use std::collections::{HashMap, HashSet, BTreeMap};
use std::vec;
use petgraph::graph::{NodeIndex, UnGraph};
use petgraph::Undirected;
use rayon::prelude::*;
use serde::{Serialize, Deserialize};
use itertools::Itertools;
use ndarray::Array2;

use crate::common::*;
use crate::common::errors::{ProcessMiningResult, ProcessMiningError};

/// Heuristic Miner implementation
pub struct HeuristicMiner {
    pub log: EventLog,
    pub params: HeuristicParameters,
    pub model: ProcessModel,
}

/// Heuristic Miner parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HeuristicParameters {
    pub dependency_threshold: f64,
    pub AND_threshold: f64,
    pub OR_threshold: f64,
    pub XOR_threshold: f64,
    pub loops_threshold: f64,
    pub fitness_threshold: f64,
    pub precision_threshold: f64,
    pub enable_performance_analysis: bool,
    pub enable_sequence_analysis: bool,
    pub enable_parallel_analysis: bool,
    pub enable_dependency_analysis: bool,
    pub enable_loop_analysis: bool,
    pub max_model_complexity: usize,
    pub significance_level: f64,
    pub confidence_level: f64,
    pub enable_noise_reduction: bool,
    pub noise_reduction_threshold: f64,
}

impl Default for HeuristicParameters {
    fn default() -> Self {
        Self {
            dependency_threshold: 0.8,
            AND_threshold: 0.6,
            OR_threshold: 0.6,
            XOR_threshold: 0.7,
            loops_threshold: 0.4,
            fitness_threshold: 0.8,
            precision_threshold: 0.7,
            enable_performance_analysis: true,
            enable_sequence_analysis: true,
            enable_parallel_analysis: true,
            enable_dependency_analysis: true,
            enable_loop_analysis: true,
            max_model_complexity: 1000,
            significance_level: 0.05,
            confidence_level: 0.95,
            enable_noise_reduction: true,
            noise_reduction_threshold: 0.1,
        }
    }
}

/// Heuristic dependencies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HeuristicDependencies {
    pub matrix: Array2<f64>,
    pub frequencies: HashMap<String, usize>,
    pub dependencies: HashMap<(String, String), f64>,
    pub dependencies_inv: HashMap<(String, String), f64>,
    pub frequency_matrix: HashMap<String, usize>,
    pub dependency_matrix: HashMap<(String, String), f64>,
    pub parallel_pairs: HashSet<(String, String)>,
    pub loop_activities: HashSet<String>,
}

/// Heuristic Miner result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HeuristicResult {
    pub model: ProcessModel,
    pub fitness: f64,
    pub precision: f64,
    pub fitness: f64,
    pub dependencies: HeuristicDependencies,
    pub computation_time: std::time::Duration,
    pub statistics: HeuristicStatistics,
    pub performance_metrics: Option<PerformanceMetrics>,
}

/// Heuristic Miner statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HeuristicStatistics {
    pub total_activities: usize,
    pub dependencies_count: usize,
    pub parallel_pairs_count: usize,
    pub loops_count: usize,
    pub model_size: usize,
    pub log_coverage: f64,
    trace_count: usize,
    event_count: usize,
    average_case_length: f64,
    noise_level: f64,
}

impl HeuristicMiner {
    /// Create new Heuristic Miner instance
    pub fn new(log: EventLog, params: HeuristicParameters) -> Self {
        Self {
            log,
            params,
            model: ProcessModel::new("heuristic_model".to_string()),
        }
    }

    /// Run the Heuristic Miner algorithm
    pub fn run(&mut self) -> ProcessMiningResult<HeuristicResult> {
        let start_time = std::time::Instant::now();

        info_pm!("heuristic", "Starting Heuristic Miner with {} cases and {} events",
            self.log.num_cases, self.log.num_events);

        // 1. Preprocess the event log
        let preprocessed = self.preprocess_log()?;

        // 2. Calculate frequencies
        let frequencies = self.calculate_frequencies(&preprocessed)?;

        // 3. Calculate dependencies
        let dependencies = self.calculate_dependencies(&preprocessed, &frequencies)?;

        // 4. Build the process model
        let model = self.build_heuristic_model(&dependencies)?;

        // 5. Evaluate the model
        let evaluation = self.evaluate_model(&model, &preprocessed)?;

        // 6. Analyze performance if enabled
        let performance_metrics = if self.params.enable_performance_analysis {
            Some(self.analyze_performance(&model, &preprocessed)?)
        } else {
            None
        };

        let computation_time = start_time.elapsed();

        let result = HeuristicResult {
            model,
            fitness: evaluation.fitness,
            precision: evaluation.precision,
            fitness: evaluation.fitness,
            dependencies,
            computation_time,
            statistics: self.calculate_statistics(&preprocessed)?,
            performance_metrics,
        };

        info_pm!("heuristic", "Heuristic Miner completed in {:?}", computation_time);
        info_pm!("heuristic", "Model fitness: {:.4}, precision: {:.4}",
            result.fitness, result.precision);

        Ok(result)
    }

    /// Preprocess the event log
    fn preprocess_log(&self) -> ProcessMiningResult<Vec<Vec<String>>> {
        debug_pm!("heuristic", "Preprocessing event log");

        let mut preprocessed = Vec::new();

        // Filter out noisy traces if enabled
        let traces = if self.params.enable_noise_reduction {
            self.filter_noisy_traces()?
        } else {
            self.get_activity_sequences()?
        };

        preprocessed.extend(traces);

        debug_pm!("heuristic", "Preprocessed {} unique traces from {} cases",
            preprocessed.len(), self.log.num_cases);

        Ok(preprocessed)
    }

    /// Filter noisy traces
    fn filter_noisy_traces(&self) -> ProcessMiningResult<Vec<Vec<String>>> {
        debug_pm!("heuristic", "Filtering noisy traces");

        let traces = self.get_activity_sequences()?;

        // Calculate frequency of each trace
        let trace_frequencies: HashMap<Vec<String>, usize> = traces
            .iter()
            .fold(HashMap::new(), |mut acc, trace| {
                *acc.entry(trace.clone()).or_insert(0) += 1;
                acc
            });

        let total_cases = trace_frequencies.values().sum::<usize>();
        let noise_threshold = self.params.noise_reduction_threshold;

        // Filter out rare traces
        let filtered_traces: Vec<Vec<String>> = trace_frequencies
            .into_iter()
            .filter(|(_trace, count)| {
                let frequency = *count as f64 / total_cases as f64;
                frequency >= noise_threshold
            })
            .map(|(trace, _)| trace)
            .collect();

        debug_pm!("heuristic", "Filtered {} traces to {} traces",
            traces.len(), filtered_traces.len());

        Ok(filtered_traces)
    }

    /// Get activity sequences from event log
    fn get_activity_sequences(&self) -> ProcessMiningResult<Vec<Vec<String>>> {
        let mut sequences = Vec::new();

        for case in self.log.cases.values() {
            let mut sequence = Vec::new();

            // Sort events by timestamp
            let mut events = case.events.clone();
            events.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

            // Extract activity sequence
            for event in events {
                sequence.push(event.activity.clone());
            }

            sequences.push(sequence);
        }

        Ok(sequences)
    }

    /// Calculate activity frequencies
    fn calculate_frequencies(&self, traces: &[Vec<String>]) -> ProcessMiningResult<HashMap<String, usize>> {
        debug_pm!("heuristic", "Calculating activity frequencies");

        let mut frequencies = HashMap::new();

        for trace in traces {
            for activity in trace {
                *frequencies.entry(activity.clone()).or_insert(0) += 1;
            }
        }

        debug_pm!("heuristic", "Calculated frequencies for {} activities",
            frequencies.len());

        Ok(frequencies)
    }

    /// Calculate dependencies between activities
    fn calculate_dependencies(&self, traces: &[Vec<String>], frequencies: &HashMap<String, usize>) -> ProcessMiningResult<HeuristicDependencies> {
        debug_pm!("heuristic", "Calculating dependencies");

        let activity_list: Vec<String> = self.log.activities.iter().cloned().collect();
        let n = activity_list.len();

        // Initialize dependency matrix
        let mut matrix = Array2::<f64>::zeros((n, n));

        // Calculate direct dependencies
        for trace in traces {
            for i in 0..trace.len() - 1 {
                let a1 = &trace[i];
                let a2 = &trace[i + 1];

                if let (Some(idx1), Some(idx2)) = (
                    activity_list.iter().position(|a| a == a1),
                    activity_list.iter().position(|a| a == a2)
                ) {
                    matrix[[idx1, idx2]] += 1.0;
                }
            }
        }

        // Convert frequencies to probabilities
        let total_events: usize = frequencies.values().sum();
        let frequencies_prob: HashMap<String, f64> = frequencies
            .iter()
            .map(|(activity, count)| (activity.clone(), *count as f64 / total_events as f64))
            .collect();

        // Calculate dependency values
        let mut dependencies = HashMap::new();
        let mut dependencies_inv = HashMap::new();
        let mut parallel_pairs = HashSet::new();

        for i in 0..n {
            for j in 0..n {
                if i != j {
                    let a1 = &activity_list[i];
                    let a2 = &activity_list[j];

                    // Calculate dependency value
                    let dep_value = if matrix[[i, j]] > 0.0 && frequencies[a1] > 0 {
                        matrix[[i, j]] / frequencies[a1] as f64
                    } else {
                        0.0
                    };

                    // Calculate inverse dependency value
                    let dep_value_inv = if matrix[[j, i]] > 0.0 && frequencies[a2] > 0 {
                        matrix[[j, i]] / frequencies[a2] as f64
                    } else {
                        0.0
                    };

                    // Check for parallel execution
                    let parallel_score = (dep_value + dep_value_inv) / 2.0;

                    if parallel_score >= self.params.AND_threshold {
                        parallel_pairs.insert((a1.clone(), a2.clone()));
                    }

                    dependencies.insert((a1.clone(), a2.clone()), dep_value);
                    dependencies_inv.insert((a1.clone(), a2.clone()), dep_value_inv);
                }
            }
        }

        // Detect loops
        let mut loop_activities = HashSet::new();
        for (i, activity) in activity_list.iter().enumerate() {
            // Simple loop detection: if activity depends on itself
            if matrix[[i, i]] > 0.0 {
                loop_activities.insert(activity.clone());
            }
        }

        let result = HeuristicDependencies {
            matrix,
            frequencies: frequencies.clone(),
            dependencies,
            dependencies_inv,
            frequency_matrix: frequencies.clone(),
            dependency_matrix: dependencies.clone(),
            parallel_pairs,
            loop_activities,
        };

        debug_pm!("heuristic", "Calculated {} dependencies, {} parallel pairs, {} loops",
            result.dependencies.len(), result.parallel_pairs.len(), result.loop_activities.len());

        Ok(result)
    }

    /// Build heuristic-based process model
    fn build_heuristic_model(&self, dependencies: &HeuristicDependencies) -> ProcessMiningResult<ProcessModel> {
        debug_pm!("heuristic", "Building heuristic model");

        let mut model = ProcessModel::new("heuristic_model".to_string());
        let mut node_map = HashMap::new();

        // Add start and end nodes
        let start_index = self.add_start_node(&mut model, &mut node_map);
        let end_index = self.add_end_node(&mut model, &mut node_map);

        // Add activity nodes
        let activity_nodes = self.add_activity_nodes(&mut model, &mut node_map, dependencies)?;

        // Add dependencies based on threshold
        self.add_dependency_edges(&mut model, &mut node_map, dependencies, &activity_nodes)?;

        // Add parallel edges
        self.add_parallel_edges(&mut model, &mut node_map, dependencies, &activity_nodes)?;

        // Add loops if detected
        self.add_loop_edges(&mut model, &mut node_map, dependencies, &activity_nodes)?;

        // Connect to start and end
        self.connect_to_start_end(&mut model, &mut node_map, dependencies, &activity_nodes, start_index, end_index)?;

        model.nodes = node_map;
        model.start_nodes = vec![start_index];
        model.end_nodes = vec![end_index];
        model.activities = self.log.activities.clone();

        debug_pm!("heuristic", "Built model with {} nodes and {} edges",
            model.graph.node_count(), model.graph.edge_count());

        Ok(model)
    }

    /// Add start node
    fn add_start_node(&self, model: &mut ProcessModel, node_map: &mut HashMap<String, NodeIndex>) -> NodeIndex {
        let start_index = model.graph.add_node(ProcessNode {
            id: "start".to_string(),
            name: Some("Start".to_string()),
            node_type: ProcessNodeType::Start,
            position: Some((0.0, 0.0)),
            labels: vec!["start".to_string()],
            properties: HashMap::new(),
        });
        node_map.insert("start".to_string(), start_index);
        start_index
    }

    /// Add end node
    fn add_end_node(&self, model: &mut ProcessModel, node_map: &mut HashMap<String, NodeIndex>) -> NodeIndex {
        let end_index = model.graph.add_node(ProcessNode {
            id: "end".to_string(),
            name: Some("End".to_string()),
            node_type: ProcessNodeType::End,
            position: Some((1.0, 0.0)),
            labels: vec!["end".to_string()],
            properties: HashMap::new(),
        });
        node_map.insert("end".to_string(), end_index);
        end_index
    }

    /// Add activity nodes
    fn add_activity_nodes(&self, model: &mut ProcessModel, node_map: &mut HashMap<String, NodeIndex>,
                         dependencies: &HeuristicDependencies) -> ProcessMiningResult<HashMap<String, NodeIndex>> {
        let mut activity_nodes = HashMap::new();
        let activities: Vec<String> = self.log.activities.iter().cloned().collect();

        for (i, activity) in activities.iter().enumerate() {
            let index = model.graph.add_node(ProcessNode {
                id: format!("activity_{}", activity),
                name: Some(activity.clone()),
                node_type: ProcessNodeType::Activity(activity.clone()),
                position: Some((i as f64 * 0.1 + 0.1, 0.0)),
                labels: vec![activity.clone()],
                properties: HashMap::new(),
            });
            node_map.insert(activity.clone(), index);
            activity_nodes.insert(activity.clone(), index);
        }

        Ok(activity_nodes)
    }

    /// Add dependency edges
    fn add_dependency_edges(&self, model: &mut ProcessModel, node_map: &mut HashMap<String, NodeIndex>,
                           dependencies: &HeuristicDependencies, activity_nodes: &HashMap<String, NodeIndex>) -> ProcessMiningResult<()> {
        for ((a1, a2), dep_value) in &dependencies.dependencies {
            if dep_value >= &self.params.dependency_threshold {
                if let (Some(a1_idx), Some(a2_idx)) = (activity_nodes.get(a1), activity_nodes.get(a2)) {
                    model.graph.add_edge(*a1_idx, *a2_idx, ProcessEdge {
                        id: format!("dep_{}_{}", a1, a2),
                        source: *a1_idx,
                        target: *a2_idx,
                        weight: Some(*dep_value),
                        conditions: vec![],
                        properties: HashMap::new(),
                    });
                }
            }
        }
        Ok(())
    }

    /// Add parallel edges
    fn add_parallel_edges(&self, model: &mut ProcessModel, node_map: &mut HashMap<String, NodeIndex>,
                         dependencies: &HeuristicDependencies, activity_nodes: &HashMap<String, NodeIndex>) -> ProcessMiningResult<()> {
        for (a1, a2) in &dependencies.parallel_pairs {
            if let (Some(a1_idx), Some(a2_idx)) = (activity_nodes.get(a1), activity_nodes.get(a2)) {
                model.graph.add_edge(*a1_idx, *a2_idx, ProcessEdge {
                    id: format!("par_{}_{}", a1, a2),
                    source: *a1_idx,
                    target: *a2_idx,
                    weight: Some(1.0),
                    conditions: vec![],
                    properties: HashMap::new(),
                });
            }
        }
        Ok(())
    }

    /// Add loop edges
    fn add_loop_edges(&self, model: &mut ProcessModel, node_map: &mut HashMap<String, NodeIndex>,
                     dependencies: &HeuristicDependencies, activity_nodes: &HashMap<String, NodeIndex>) -> ProcessMiningResult<()> {
        for activity in &dependencies.loop_activities {
            if let Some(activity_idx) = activity_nodes.get(activity) {
                // Add self-loop
                model.graph.add_edge(*activity_idx, *activity_idx, ProcessEdge {
                    id: format!("loop_{}", activity),
                    source: *activity_idx,
                    target: *activity_idx,
                    weight: Some(1.0),
                    conditions: vec![],
                    properties: HashMap::new(),
                });
            }
        }
        Ok(())
    }

    /// Connect nodes to start and end
    fn connect_to_start_end(&self, model: &mut ProcessModel, node_map: &mut HashMap<String, NodeIndex>,
                           dependencies: &HeuristicDependencies, activity_nodes: &HashMap<String, NodeIndex>,
                           start_index: NodeIndex, end_index: NodeIndex) -> ProcessMiningResult<()> {
        let start_activities: Vec<String> = self.log.activities.iter()
            .filter(|activity| {
                if let Some(idx) = activity_nodes.get(activity) {
                    dependencies.dependencies.get(&("start".to_string(), activity.clone()))
                        .map(|dep| dep >= &self.params.dependency_threshold)
                        .unwrap_or(false)
                } else {
                    false
                }
            })
            .cloned()
            .collect();

        for activity in start_activities {
            if let Some(activity_idx) = activity_nodes.get(&activity) {
                model.graph.add_edge(start_index, *activity_idx, ProcessEdge {
                    id: format!("start_{}", activity),
                    source: start_index,
                    target: *activity_idx,
                    weight: Some(1.0),
                    conditions: vec![],
                    properties: HashMap::new(),
                });
            }
        }

        let end_activities: Vec<String> = self.log.activities.iter()
            .filter(|activity| {
                if let Some(idx) = activity_nodes.get(activity) {
                    dependencies.dependencies_inv.get(&("end".to_string(), activity.clone()))
                        .map(|dep| dep >= &self.params.dependency_threshold)
                        .unwrap_or(false)
                } else {
                    false
                }
            })
            .cloned()
            .collect();

        for activity in end_activities {
            if let Some(activity_idx) = activity_nodes.get(&activity) {
                model.graph.add_edge(*activity_idx, end_index, ProcessEdge {
                    id: format!("end_{}", activity),
                    source: *activity_idx,
                    target: end_index,
                    weight: Some(1.0),
                    conditions: vec![],
                    properties: HashMap::new(),
                });
            }
        }

        Ok(())
    }

    /// Evaluate the model fitness
    fn evaluate_model(&self, model: &ProcessModel, traces: &[Vec<String>]) -> ProcessMiningResult<ModelEvaluation> {
        debug_pm!("heuristic", "Evaluating model fitness");

        let mut total_fitness = 0.0;
        let mut total_precision = 0.0;
        let trace_count = traces.len();

        for trace in traces {
            let fitness = self.calculate_trace_fitness(model, trace)?;
            let precision = self.calculate_trace_precision(model, trace)?;

            total_fitness += fitness;
            total_precision += precision;
        }

        let fitness = if trace_count > 0 {
            total_fitness / trace_count as f64
        } else {
            0.0
        };

        let precision = if trace_count > 0 {
            total_precision / trace_count as f64
        } else {
            0.0
        };

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
        let mut correct_transitions = 0;
        let mut total_transitions = 0;

        for i in 0..trace.len() - 1 {
            let activity1 = &trace[i];
            let activity2 = &trace[i + 1];

            if self.model_has_path(model, activity1, activity2) {
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

    /// Calculate trace precision
    fn calculate_trace_precision(&self, model: &ProcessModel, trace: &[String]) -> ProcessMiningResult<f64> {
        // Simplified precision calculation
        let mut expected_behaviors = HashSet::new();
        let mut model_behaviors = HashSet::new();

        // Trace behaviors
        for i in 0..trace.len() - 1 {
            let pair = (trace[i].clone(), trace[i + 1].clone());
            expected_behaviors.insert(pair);
        }

        // Model behaviors (simplified)
        for (node1_idx, node1_data) in model.graph.node_indices().zip(&model.graph) {
            for (node2_idx, node2_data) in model.graph.node_indices().zip(&model.graph) {
                if model.graph.contains_edge(node1_idx, node2_idx) {
                    if let (Some(name1), Some(name2)) = (
                        get_activity_name(&node1_data.node_type),
                        get_activity_name(&node2_data.node_type)
                    ) {
                        model_behaviors.insert((name1.clone(), name2.clone()));
                    }
                }
            }
        }

        let precision = if model_behaviors.is_empty() {
            1.0
        } else {
            let intersection = expected_behaviors.intersection(&model_behaviors).count();
            intersection as f64 / expected_behaviors.len() as f64
        };

        Ok(precision.max(0.0).min(1.0))
    }

    /// Check if model has path between activities
    fn model_has_path(&self, model: &ProcessModel, activity1: &str, activity2: &str) -> bool {
        if let (Some(a1_idx), Some(a2_idx)) = (model.nodes.get(activity1), model.nodes.get(activity2)) {
            // Check if there's a path from a1 to a2
            // This is a simplified check - in production, use proper graph traversal
            model.graph.contains_edge(*a1_idx, *a2_idx)
        } else {
            false
        }
    }

    /// Calculate generalization
    fn calculate_generalization(&self, model: &ProcessModel, traces: &[Vec<String>]) -> ProcessMiningResult<f64> {
        // Simplified generalization calculation
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
        let nodes = model.graph.node_count();
        let edges = model.graph.edge_count();

        if nodes == 0 {
            1.0
        } else {
            1.0 / (1.0 + edges as f64 / nodes as f64)
        }
    }

    /// Analyze performance
    fn analyze_performance(&self, model: &ProcessModel, traces: &[Vec<String>]) -> ProcessMiningResult<PerformanceMetrics> {
        debug_pm!("heuristic", "Analyzing model performance");

        let metrics = PerformanceMetrics::new();

        // Calculate throughput
        let total_events = traces.iter().map(|trace| trace.len()).sum::<usize>();
        let total_cases = traces.len();
        let avg_case_length = total_events as f64 / total_cases as f64;

        metrics.throughput = (total_cases as f64 * 3600.0) / avg_case_length as f64;
        metrics.average_case_duration = chrono::Duration::seconds(avg_case_length as i64);
        metrics.utilization = 0.8; // Simplified

        Ok(metrics)
    }

    /// Calculate statistics
    fn calculate_statistics(&self, traces: &[Vec<String>]) -> ProcessMiningResult<HeuristicStatistics> {
        let mut total_events = 0;
        let mut min_length = usize::MAX;
        let mut max_length = 0;

        for trace in traces {
            let length = trace.len();
            total_events += length;
            min_length = min_length.min(length);
            max_length = max_length.max(length);
        }

        let trace_count = traces.len();
        let average_case_length = total_events as f64 / trace_count as f64;

        // Calculate noise level
        let noise_level = self.calculate_noise_level(traces);

        Ok(HeuristicStatistics {
            total_activities: self.log.activities.len(),
            dependencies_count: 0, // Will be set later
            parallel_pairs_count: 0, // Will be set later
            loops_count: 0, // Will be set later
            model_size: 0, // Will be set later
            log_coverage: 1.0, // Simplified
            trace_count,
            event_count: total_events,
            average_case_length,
            noise_level,
        })
    }

    /// Calculate noise level
    fn calculate_noise_level(&self, traces: &[Vec<String>]) -> f64 {
        // Simplified noise calculation based on trace length variance
        let lengths: Vec<f64> = traces.iter().map(|trace| trace.len() as f64).collect();
        let mean = lengths.iter().sum::<f64>() / lengths.len() as f64;
        let variance = lengths.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / lengths.len() as f64;
        variance.sqrt() / mean
    }

    /// Get model information
    pub fn get_model_info(&self) -> ModelInfo {
        ModelInfo {
            algorithm_name: "Heuristic Miner".to_string(),
            algorithm_version: "1.0".to_string(),
            input_size: self.log.num_cases,
            output_size: self.model.graph.node_count() + self.model.graph.edge_count(),
            computation_time: std::time::Instant::now().elapsed(),
            memory_usage: self.get_memory_usage(),
        }
    }

    /// Get memory usage
    fn get_memory_usage(&self) -> u64 {
        let base_size = std::mem::size_of_val(self);
        let log_size = self.log.num_cases * 100;
        let model_size = self.model.graph.node_count() * 100;
        (base_size + log_size + model_size) as u64
    }
}

/// Get activity name from node type
fn get_activity_name(node_type: &ProcessNodeType) -> Option<&str> {
    match node_type {
        ProcessNodeType::Activity(activity) => Some(activity),
        _ => None,
    }
}

/// Heuristic Miner utilities
pub mod utils {
    use super::*;

    /// Generate test event log for testing
    pub fn generate_test_event_log(num_cases: usize, case_length: usize) -> EventLog {
        let mut log = EventLog::new("test_log".to_string());

        let activities = vec!["A", "B", "C", "D", "E", "F"];

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

    /// Create simple test dependencies
    pub fn create_test_dependencies() -> HeuristicDependencies {
        let activities = vec!["A", "B", "C", "D"];
        let n = activities.len();
        let matrix = Array2::<f64>::zeros((n, n));

        let mut dependencies = HashMap::new();
        let mut dependencies_inv = HashMap::new();
        let mut parallel_pairs = HashSet::new();

        // Add some dependencies
        dependencies.insert(("A".to_string(), "B".to_string()), 0.9);
        dependencies.insert(("B".to_string(), "C".to_string()), 0.8);
        dependencies.insert(("C".to_string(), "D".to_string()), 0.7);

        // Add inverse dependencies
        dependencies_inv.insert(("B".to_string(), "A".to_string()), 0.1);
        dependencies_inv.insert(("C".to_string(), "B".to_string()), 0.2);
        dependencies_inv.insert(("D".to_string(), "C".to_string()), 0.3);

        // Add parallel pairs
        parallel_pairs.insert(("A".to_string(), "C".to_string()));
        parallel_pairs.insert(("B".to_string(), "D".to_string()));

        HeuristicDependencies {
            matrix,
            frequencies: activities.iter().map(|a| (a.to_string(), 10)).collect(),
            dependencies,
            dependencies_inv,
            frequency_matrix: activities.iter().map(|a| (a.to_string(), 10)).collect(),
            dependency_matrix: dependencies.clone(),
            parallel_pairs,
            loop_activities: HashSet::new(),
        }
    }

    /// Validate heuristic dependencies
    pub fn validate_dependencies(dependencies: &HeuristicDependencies) -> bool {
        // Check that all dependencies are within [0, 1]
        for value in dependencies.dependencies.values() {
            if *value < 0.0 || *value > 1.0 {
                return false;
            }
        }

        // Check that inverse dependencies are within [0, 1]
        for value in dependencies.dependencies_inv.values() {
            if *value < 0.0 || *value > 1.0 {
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
    fn test_heuristic_miner_creation() {
        let log = EventLog::new("test_log".to_string());
        let params = HeuristicParameters::default();
        let heuristic = HeuristicMiner::new(log, params);

        assert_eq!(heuristic.model.id, "heuristic_model");
    }

    #[test]
    fn test_activity_frequency_calculation() {
        let traces = vec![
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            vec!["A".to_string(), "C".to_string()],
            vec!["B".to_string(), "C".to_string()],
        ];

        let log = EventLog::new("test".to_string());
        log.activities.insert("A".to_string());
        log.activities.insert("B".to_string());
        log.activities.insert("C".to_string());

        let params = HeuristicParameters::default();
        let heuristic = HeuristicMiner::new(log, params);

        let frequencies = heuristic.calculate_frequencies(&traces).unwrap();

        assert_eq!(frequencies["A"], 2);
        assert_eq!(frequencies["B"], 2);
        assert_eq!(frequencies["C"], 3);
    }

    #[test]
    fn test_dependency_calculation() {
        let traces = vec![
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            vec!["A".to_string(), "C".to_string()],
            vec!["B".to_string(), "C".to_string()],
        ];

        let log = EventLog::new("test".to_string());
        log.activities.insert("A".to_string());
        log.activities.insert("B".to_string());
        log.activities.insert("C".to_string());

        let params = HeuristicParameters::default();
        let heuristic = HeuristicMiner::new(log, params);

        let frequencies = HashMap::from([
            ("A".to_string(), 2),
            ("B".to_string(), 2),
            ("C".to_string(), 3),
        ]);

        let dependencies = heuristic.calculate_dependencies(&traces, &frequencies).unwrap();

        // Check that A->B dependency exists
        assert_eq!(dependencies.dependencies.get(&("A".to_string(), "B".to_string())), Some(&0.5));

        // Check that A->C dependency exists
        assert_eq!(dependencies.dependencies.get(&("A".to_string(), "C".to_string())), Some(&0.5));
    }

    #[test]
    fn test_dependency_validation() {
        let dependencies = utils::create_test_dependencies();
        assert!(utils::validate_dependencies(&dependencies));
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
    fn test_trace_fitness_calculation() {
        let log = EventLog::new("test".to_string());
        let params = HeuristicParameters::default();
        let heuristic = HeuristicMiner::new(log, params);

        let traces = vec![
            vec!["A".to_string(), "B".to_string()],
            vec!["A".to_string(), "C".to_string()],
        ];

        let model = ProcessModel::new("test".to_string());

        let fitness = heuristic.calculate_trace_fitness(&model, &traces[0]).unwrap();
        assert!(fitness >= 0.0 && fitness <= 1.0);

        let precision = heuristic.calculate_trace_precision(&model, &traces[0]).unwrap();
        assert!(precision >= 0.0 && precision <= 1.0);
    }

    #[test]
    fn test_noise_level_calculation() {
        let log = EventLog::new("test".to_string());
        let params = HeuristicParameters::default();
        let heuristic = HeuristicMiner::new(log, params);

        let traces = vec![
            vec!["A".to_string(), "B".to_string()],
            vec!["A".to_string(), "B".to_string(), "C".to_string()],
            vec!["A".to_string(), "C".to_string()],
            vec!["B".to_string(), "C".to_string(), "D".to_string()],
        ];

        let noise_level = heuristic.calculate_noise_level(&traces);
        assert!(noise_level >= 0.0);
    }

    #[test]
    fn test_statistics_calculation() {
        let log = EventLog::new("test_log".to_string());
        log.activities.insert("A".to_string());
        log.activities.insert("B".to_string());
        log.activities.insert("C".to_string());

        let params = HeuristicParameters::default();
        let heuristic = HeuristicMiner::new(log, params);

        let traces = vec![
            vec!["A".to_string(), "B".to_string()],
            vec!["A".to_string(), "C".to_string()],
        ];

        let stats = heuristic.calculate_statistics(&traces).unwrap();
        assert_eq!(stats.total_activities, 3);
        assert_eq!(stats.trace_count, 2);
        assert_eq!(stats.event_count, 4);
        assert_eq!(stats.average_case_length, 2.0);
        assert!(stats.noise_level >= 0.0);
    }
}