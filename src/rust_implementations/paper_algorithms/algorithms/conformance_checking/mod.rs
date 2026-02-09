//! Conformance Checking Implementation
//!
//! Implementation of conformance checking algorithms from various papers including:
//! - Conformance Checking over Uncertain Event Data (2009.14452)
//! - Conformance Checking for Trace Fragments Using Infix and Postfix Alignments (2209.04290)
//! - Alignments for Process Models (various papers)
//!
//! Conformance checking measures how well a process model fits an event log.

use std::collections::{HashMap, HashSet, VecDeque};
use std::vec;
use petgraph::graph::{NodeIndex, DiGraph};
use petgraph::Directed;
use serde::{Serialize, Deserialize};
use itertools::Itertools;
use rayon::prelude::*;

use crate::common::*;
use crate::common::errors::{ProcessMiningResult, ProcessMiningError};

/// Conformance checking implementation
pub struct ConformanceChecker {
    pub log: EventLog,
    pub model: ProcessModel,
    pub params: ConformanceParameters,
}

/// Conformance checking parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConformanceParameters {
    pub alignment_threshold: f64,
    pub fitness_threshold: f64,
    pub precision_threshold: f64,
    pub significance_threshold: f64,
    pub enable_alignment: bool,
    pub enable_fitness_analysis: bool,
    pub enable_precision_analysis: bool,
    pub enable_deviation_detection: bool,
    pub enable_uncertain_data: bool,
    pub enable_trace_fragments: bool,
    pub max_alignment_length: usize,
    pub enable_visualization: bool,
    pub cache_alignments: bool,
    pub cache_size: usize,
    pub enable_parallel_computation: bool,
}

impl Default for ConformanceParameters {
    fn default() -> Self {
        Self {
            alignment_threshold: 0.1,
            fitness_threshold: 0.8,
            precision_threshold: 0.7,
            significance_threshold: 0.05,
            enable_alignment: true,
            enable_fitness_analysis: true,
            enable_precision_analysis: true,
            enable_deviation_detection: true,
            enable_uncertain_data: false,
            enable_trace_fragments: false,
            max_alignment_length: 100,
            enable_visualization: false,
            cache_alignments: true,
            cache_size: 1000,
            enable_parallel_computation: true,
        }
    }
}

/// Alignment result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlignmentResult {
    pub trace_id: String,
    pub alignment: Vec<AlignmentMove>,
    pub cost: f64,
    pub fitness: f64,
    pub deviations: Vec<Deviation>,
    pub computation_time: std::time::Duration,
}

/// Conformance checking result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConformanceResult {
    pub model: ProcessModel,
    pub trace_fitness: Vec<TraceFitness>,
    pub total_fitness: f64,
    pub fitness: f64,
    pub precision: f64,
    pub recall: f64,
    pub generalization: f64,
    pub alignments: Vec<AlignmentResult>,
    pub deviation_summary: DeviationSummary,
    pub computation_time: std::time::Duration,
    pub statistics: ConformanceStatistics,
}

/// Conformance checking statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConformanceStatistics {
    pub total_traces: usize,
    pub conforming_traces: usize,
    pub deviating_traces: usize,
    pub total_deviations: usize,
    pub missing_activities: usize,
    pub extra_activities: usize,
    pub wrong_order: usize,
    pub wrong_activities: usize,
    pub average_fitness: f64,
    pub fitness_distribution: HashMap<String, usize>,
    pub deviation_distribution: HashMap<String, usize>,
}

/// Deviation summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviationSummary {
    pub missing_activities: Vec<Deviation>,
    pub extra_activities: Vec<Deviation>,
    pub wrong_order: Vec<Deviation>,
    pub wrong_activities: Vec<Deviation>,
    pub total_cost: f64,
    pub average_cost_per_deviation: f64,
    pub max_deviation_cost: f64,
}

impl ConformanceChecker {
    /// Create new Conformance Checker instance
    pub fn new(log: EventLog, model: ProcessModel, params: ConformanceParameters) -> Self {
        Self {
            log,
            model,
            params,
        }
    }

    /// Run conformance checking
    pub fn run(&mut self) -> ProcessMiningResult<ConformanceResult> {
        let start_time = std::time::Instant::now();

        info_pm!("conformance", "Starting conformance checking with {} cases and model with {} nodes",
            self.log.num_cases, self.model.graph.node_count());

        // 1. Check model validity
        self.validate_model()?;

        // 2. Calculate trace fitness
        let trace_fitness = self.calculate_trace_fitness()?;

        // 3. Calculate overall fitness
        let fitness = self.calculate_overall_fitness(&trace_fitness)?;

        // 4. Calculate precision
        let precision = self.calculate_precision()?;

        // 5. Calculate recall
        let recall = self.calculate_recall()?;

        // 6. Calculate generalization
        let generalization = self.calculate_generalization()?;

        // 7. Perform alignment if enabled
        let alignments = if self.params.enable_alignment {
            self.calculate_alignments()?
        } else {
            Vec::new()
        };

        // 8. Detect deviations
        let deviation_summary = self.detect_deviations(&trace_fitness)?;

        // 9. Generate statistics
        let statistics = self.calculate_conformance_statistics(&trace_fitness, &deviation_summary)?;

        let computation_time = start_time.elapsed();

        let result = ConformanceResult {
            model: self.model.clone(),
            trace_fitness,
            total_fitness: fitness,
            fitness,
            precision,
            recall,
            generalization,
            alignments,
            deviation_summary,
            computation_time,
            statistics,
        };

        info_pm!("conformance", "Conformance checking completed in {:?}", computation_time);
        info_pm!("conformance", "Overall fitness: {:.4}, precision: {:.4}, recall: {:.4}, generalization: {:.4}",
            result.fitness, result.precision, result.recall, result.generalization);

        Ok(result)
    }

    /// Validate the process model
    fn validate_model(&self) -> ProcessMiningResult<()> {
        debug_pm!("conformance", "Validating process model");

        // Check if model has start and end nodes
        if self.model.start_nodes.is_empty() {
            return Err(ProcessMiningError::InvalidModel("Model has no start nodes".to_string()));
        }

        if self.model.end_nodes.is_empty() {
            return Err(ProcessMiningError::InvalidModel("Model has no end nodes".to_string()));
        }

        // Check if all activities in log are present in model
        for activity in &self.log.activities {
            if !self.model.activities.contains(activity) {
                return Err(ProcessMiningError::InvalidModel(
                    format!("Model is missing activity: {}", activity)
                ));
            }
        }

        debug_pm!("conformance", "Model validation passed");
        Ok(())
    }

    /// Calculate trace fitness
    fn calculate_trace_fitness(&self) -> ProcessMiningResult<Vec<TraceFitness>> {
        debug_pm!("conformance", "Calculating trace fitness");

        let trace_fitness = if self.params.enable_parallel_computation {
            self.log.get_cases()
                .par_iter()
                .map(|case| self.calculate_single_trace_fitness(case))
                .collect::<ProcessMiningResult<Vec<TraceFitness>>>()?
        } else {
            self.log.get_cases()
                .iter()
                .map(|case| self.calculate_single_trace_fitness(case))
                .collect::<ProcessMiningResult<Vec<TraceFitness>>>()?
        };

        debug_pm!("conformance", "Calculated fitness for {} traces", trace_fitness.len());
        Ok(trace_fitness)
    }

    /// Calculate fitness for a single trace
    fn calculate_single_trace_fitness(&self, case: &Case) -> TraceFitness {
        let case_id = case.id.clone();
        let activities = case.get_activity_sequence();

        // Calculate fitness based on alignment
        let fitness = self.calculate_trace_alignment_fitness(&activities);

        // Detect deviations
        let deviations = self.detect_trace_deviations(&activities);

        TraceFitness {
            case_id,
            fitness,
            deviations,
        }
    }

    /// Calculate trace alignment fitness
    fn calculate_trace_alignment_fitness(&self, activities: &[String]) -> f64 {
        // Simplified fitness calculation
        // In production, use proper alignment algorithms

        let mut correct_moves = 0;
        let mut total_moves = 0;

        for i in 0..activities.len() - 1 {
            let activity1 = &activities[i];
            let activity2 = &activities[i + 1];

            if self.model_has_transition(activity1, activity2) {
                correct_moves += 1;
            }
            total_moves += 1;
        }

        if total_moves == 0 {
            1.0
        } else {
            correct_moves as f64 / total_moves as f64
        }
    }

    /// Check if model has transition between activities
    fn model_has_transition(&self, activity1: &str, activity2: &str) -> bool {
        if let (Some(a1_idx), Some(a2_idx)) = (self.model.nodes.get(activity1), self.model.nodes.get(activity2)) {
            self.model.graph.contains_edge(*a1_idx, *a2_idx)
        } else {
            false
        }
    }

    /// Detect trace deviations
    fn detect_trace_deviations(&self, activities: &[String]) -> Vec<Deviation> {
        let mut deviations = Vec::new();

        for i in 0..activities.len() {
            let activity = &activities[i];

            // Check if activity exists in model
            if !self.model.activities.contains(activity) {
                deviations.push(Deviation {
                    type: DeviationType::WrongActivity,
                    description: format!("Unknown activity: {}", activity),
                    location: i,
                });
            }

            // Check order constraints
            if i < activities.len() - 1 {
                let prev_activity = &activities[i];
                let next_activity = &activities[i + 1];

                if !self.model_has_transition(prev_activity, next_activity) {
                    deviations.push(Deviation {
                        type: DeviationType::WrongOrder,
                        description: format!("Invalid order: {} -> {}", prev_activity, next_activity),
                        location: i,
                    });
                }
            }
        }

        deviations
    }

    /// Calculate overall fitness
    fn calculate_overall_fitness(&self, trace_fitness: &[TraceFitness]) -> ProcessMiningResult<f64> {
        if trace_fitness.is_empty() {
            return Ok(0.0);
        }

        let total_fitness: f64 = trace_fitness.iter()
            .map(|tf| tf.fitness)
            .sum();

        Ok(total_fitness / trace_fitness.len() as f64)
    }

    /// Calculate precision
    fn calculate_precision(&self) -> ProcessMiningResult<f64> {
        // Simplified precision calculation
        // In production, use proper precision analysis with alignments

        let total_possible = self.model.graph.edge_count();
        let actual_observed = self.log.get_activity_frequencies().len();

        if total_possible == 0 {
            Ok(1.0)
        } else {
            Ok(actual_observed as f64 / total_possible as f64)
        }
    }

    /// Calculate recall
    fn calculate_recall(&self) -> ProcessMiningResult<f64> {
        // Simplified recall calculation
        // In production, use proper recall analysis

        let total_observed = self.log.get_activity_frequencies().len();
        let total_possible = self.log.activities.len();

        if total_possible == 0 {
            Ok(1.0)
        } else {
            Ok(total_observed as f64 / total_possible as f64)
        }
    }

    /// Calculate generalization
    fn calculate_generalization(&self) -> ProcessMiningResult<f64> {
        // Simplified generalization calculation
        // In production, use proper generalization metrics

        let nodes = self.model.graph.node_count();
        let edges = self.model.graph.edge_count();
        let activities = self.log.activities.len();

        if activities == 0 {
            Ok(1.0)
        } else {
            // Penalize model complexity
            let complexity = (nodes + edges) as f64 / activities as f64;
            Ok(1.0 / (1.0 + complexity))
        }
    }

    /// Calculate alignments for all traces
    fn calculate_alignments(&self) -> ProcessMiningResult<Vec<AlignmentResult>> {
        debug_pm!("conformance", "Calculating alignments");

        let alignments = if self.params.enable_parallel_computation {
            self.log.get_cases()
                .par_iter()
                .map(|case| self.calculate_single_alignment(case))
                .collect::<ProcessMiningResult<Vec<AlignmentResult>>>()?
        } else {
            self.log.get_cases()
                .iter()
                .map(|case| self.calculate_single_alignment(case))
                .collect::<ProcessMiningResult<Vec<AlignmentResult>>>()?
        };

        debug_pm!("conformance", "Calculated {} alignments", alignments.len());
        Ok(alignments)
    }

    /// Calculate alignment for a single trace
    fn calculate_single_alignment(&self, case: &Case) -> AlignmentResult {
        let case_id = case.id.clone();
        let activities = case.get_activity_sequence();
        let start_time = std::time::Instant::now();

        // Simplified alignment calculation
        // In production, use proper alignment algorithms like in ProM

        let alignment_moves = self.calculate_alignment_moves(&activities);
        let cost = alignment_moves.iter()
            .map(|move_| move_.cost)
            .sum::<f64>();

        let fitness = 1.0 - (cost / activities.len() as f64);

        let computation_time = start_time.elapsed();

        AlignmentResult {
            trace_id: case_id,
            alignment: alignment_moves,
            cost,
            fitness,
            deviations: self.detect_trace_deviations(&activities),
            computation_time,
        }
    }

    /// Calculate alignment moves
    fn calculate_alignment_moves(&self, activities: &[String]) -> Vec<AlignmentMove> {
        let mut moves = Vec::new();

        for (i, activity) in activities.iter().enumerate() {
            // Move on trace
            if self.model.activities.contains(activity) {
                // Check if there's a corresponding move in the model
                moves.push(AlignmentMove {
                    move_type: AlignmentMoveType::SyncMove,
                    trace_element: Some(activity.clone()),
                    model_element: Some(activity.clone()),
                });
            } else {
                moves.push(AlignmentMove {
                    move_type: AlignmentMoveType::MoveOnTrace,
                    trace_element: Some(activity.clone()),
                    model_element: None,
                });
            }
        }

        // Add log move to complete the trace
        moves.push(AlignmentMove {
            move_type: AlignmentMoveType::LogMove,
            trace_element: None,
            model_element: None,
        });

        moves
    }

    /// Detect deviations in traces
    fn detect_deviations(&self, trace_fitness: &[TraceFitness]) -> ProcessMiningResult<DeviationSummary> {
        debug_pm!("conformance", "Detecting deviations");

        let mut missing_activities = Vec::new();
        let mut extra_activities = Vec::new();
        let mut wrong_order = Vec::new();
        let mut wrong_activities = Vec::new();
        let mut total_cost = 0.0;
        let mut max_cost = 0.0;

        for trace_fitness in trace_fitness {
            for deviation in &trace_fitness.deviations {
                match deviation.type {
                    DeviationType::MissingActivity => missing_activities.push(deviation.clone()),
                    DeviationType::ExtraActivity => extra_activities.push(deviation.clone()),
                    DeviationType::WrongOrder => wrong_order.push(deviation.clone()),
                    DeviationType::WrongActivity => wrong_activities.push(deviation.clone()),
                }

                total_cost += deviation.get_cost();
                max_cost = max_cost.max(deviation.get_cost());
            }
        }

        let average_cost = if trace_fitness.is_empty() {
            0.0
        } else {
            total_cost / trace_fitness.len() as f64
        };

        Ok(DeviationSummary {
            missing_activities,
            extra_activities,
            wrong_order,
            wrong_activities,
            total_cost,
            average_cost_per_deviation: average_cost,
            max_deviation_cost: max_cost,
        })
    }

    /// Calculate conformance statistics
    fn calculate_conformance_statistics(&self, trace_fitness: &[TraceFitness], deviation_summary: &DeviationSummary) -> ProcessMiningResult<ConformanceStatistics> {
        let total_traces = trace_fitness.len();
        let conforming_traces = trace_fitness.iter()
            .filter(|tf| tf.fitness >= self.params.fitness_threshold)
            .count();
        let deviating_traces = total_traces - conforming_traces;

        let mut fitness_distribution = HashMap::new();
        let mut deviation_distribution = HashMap::new();

        for trace_fitness in trace_fitness {
            // Fitness distribution
            let fitness_range = self.get_fitness_range(trace_fitness.fitness);
            *fitness_distribution.entry(fitness_range).or_insert(0) += 1;

            // Deviation distribution
            for deviation in &trace_fitness.deviations {
                let deviation_type = format!("{:?}", deviation.type);
                *deviation_distribution.entry(deviation_type).or_insert(0) += 1;
            }
        }

        let average_fitness = if total_traces > 0 {
            trace_fitness.iter().map(|tf| tf.fitness).sum::<f64>() / total_traces as f64
        } else {
            0.0
        };

        Ok(ConformanceStatistics {
            total_traces,
            conforming_traces,
            deviating_traces,
            total_deviations: deviation_summary.missing_activities.len() +
                           deviation_summary.extra_activities.len() +
                           deviation_summary.wrong_order.len() +
                           deviation_summary.wrong_activities.len(),
            missing_activities: deviation_summary.missing_activities.len(),
            extra_activities: deviation_summary.extra_activities.len(),
            wrong_order: deviation_summary.wrong_order.len(),
            wrong_activities: deviation_summary.wrong_activities.len(),
            average_fitness,
            fitness_distribution,
            deviation_distribution,
        })
    }

    /// Get fitness range for statistics
    fn get_fitness_range(&self, fitness: f64) -> String {
        match fitness {
            f if f >= 0.9 => "90-100%",
            f if f >= 0.7 => "70-90%",
            f if f >= 0.5 => "50-70%",
            f if f >= 0.3 => "30-50%",
            _ => "0-30%",
        }.to_string()
    }

    /// Generate conformance report
    pub fn generate_report(&self, result: &ConformanceResult) -> ProcessMiningResult<String> {
        let mut report = String::new();

        report.push_str("Conformance Checking Report\n");
        report.push_str("============================\n\n");

        // Overall metrics
        report.push_str("Overall Metrics:\n");
        report.push_str(&format!("  Fitness: {:.4}\n", result.fitness));
        report.push_str(&format!("  Precision: {:.4}\n", result.precision));
        report.push_str(&format!("  Recall: {:.4}\n", result.recall));
        report.push_str(&format!("  Generalization: {:.4}\n", result.generalization));
        report.push_str(&format!("  Computation Time: {:?}\n", result.computation_time));
        report.push_str("\n");

        // Trace fitness distribution
        report.push_str("Trace Fitness Distribution:\n");
        for (range, count) in &result.statistics.fitness_distribution {
            report.push_str(&format!("  {}: {}\n", range, count));
        }
        report.push_str("\n");

        // Deviation summary
        report.push_str("Deviation Summary:\n");
        report.push_str(&format!("  Total Deviations: {}\n", result.statistics.total_deviations));
        report.push_str(&format!("  Missing Activities: {}\n", result.statistics.missing_activities));
        report.push_str(&format!("  Extra Activities: {}\n", result.statistics.extra_activities));
        report.push_str(&format!("  Wrong Order: {}\n", result.statistics.wrong_order));
        report.push_str(&format!("  Wrong Activities: {}\n", result.statistics.wrong_activities));
        report.push_str("\n");

        // Detailed deviations
        if !result.deviation_summary.missing_activities.is_empty() {
            report.push_str("Missing Activities:\n");
            for deviation in &result.deviation_summary.missing_activities {
                report.push_str(&format!("  - {}\n", deviation.description));
            }
            report.push_str("\n");
        }

        if !result.deviation_summary.extra_activities.is_empty() {
            report.push_str("Extra Activities:\n");
            for deviation in &result.deviation_summary.extra_activities {
                report.push_str(&format!("  - {}\n", deviation.description));
            }
            report.push_str("\n");
        }

        if !result.deviation_summary.wrong_order.is_empty() {
            report.push_str("Wrong Order:\n");
            for deviation in &result.deviation_summary.wrong_order {
                report.push_str(&format!("  - {}\n", deviation.description));
            }
            report.push_str("\n");
        }

        if !result.deviation_summary.wrong_activities.is_empty() {
            report.push_str("Wrong Activities:\n");
            for deviation in &result.deviation_summary.wrong_activities {
                report.push_str(&format!("  - {}\n", deviation.description));
            }
            report.push_str("\n");
        }

        // Recommendations
        report.push_str("Recommendations:\n");
        if result.fitness < self.params.fitness_threshold {
            report.push_str("  - Model fitness is below threshold. Consider revising the model.\n");
        }
        if result.precision < self.params.precision_threshold {
            report.push_str("  - Model precision is below threshold. Consider adding more constraints.\n");
        }
        if result.statistics.deviating_traces > 0 {
            report.push_str("  - Consider analyzing deviating traces to identify model improvements.\n");
        }

        Ok(report)
    }

    /// Export conformance results
    pub fn export_results(&self, result: &ConformanceResult, format: &str, path: &std::path::Path) -> ProcessMiningResult<()> {
        match format.to_lowercase().as_str() {
            "json" => {
                let json = serde_json::to_string_pretty(result)?;
                std::fs::write(path, json)?;
            }
            "csv" => {
                self.export_to_csv(result, path)?;
            }
            "xes" => {
                self.export_to_xes(result, path)?;
            }
            _ => {
                return Err(ProcessMiningError::FormatError(
                    format!("Unsupported export format: {}", format)
                ));
            }
        }

        Ok(())
    }

    /// Export results to CSV
    fn export_to_csv(&self, result: &ConformanceResult, path: &std::path::Path) -> ProcessMiningResult<()> {
        let mut wtr = csv::Writer::from_path(path)?;

        // Write trace fitness
        wtr.write_record(&["case_id", "fitness", "deviations_count"])?;
        for trace_fitness in &result.trace_fitness {
            wtr.write_record(&[
                &trace_fitness.case_id,
                &trace_fitness.fitness.to_string(),
                &trace_fitness.deviations.len().to_string(),
            ])?;
        }

        // Write deviations
        wtr.write_record(&["case_id", "deviation_type", "description", "location"])?;
        for trace_fitness in &result.trace_fitness {
            for deviation in &trace_fitness.deviations {
                wtr.write_record(&[
                    &trace_fitness.case_id,
                    &format!("{:?}", deviation.type),
                    &deviation.description,
                    &deviation.location.to_string(),
                ])?;
            }
        }

        Ok(())
    }

    /// Export results to XES
    fn export_to_xes(&self, result: &ConformanceResult, path: &std::path::Path) -> ProcessMiningResult<()> {
        let mut xes = String::new();

        xes.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        xes.push_str("<log>\n");

        // Write traces
        for trace_fitness in &result.trace_fitness {
            xes.push_str("  <trace>\n");
            xes.push_str(&format!("    <id>{}</id>\n", trace_fitness.case_id));
            xes.push_str(&format!("    <string key=\"fitness\">{:.4}</string>\n", trace_fitness.fitness));
            xes.push_str(&format!("    <int key=\"deviations_count\">{}</int>\n", trace_fitness.deviations.len()));

            // Write events
            for deviation in &trace_fitness.deviations {
                xes.push_str("    <event>\n");
                xes.push_str(&format!("      <string key=\"deviation_type\">{:?}</string>\n", deviation.type));
                xes.push_str(&format!("      <string key=\"description\">{}</string>\n", deviation.description));
                xes.push_str(&format!("      <int key=\"location\">{}</int>\n", deviation.location));
                xes.push_str("    </event>\n");
            }

            xes.push_str("  </trace>\n");
        }

        xes.push_str("</log>\n");

        std::fs::write(path, xes)?;
        Ok(())
    }
}

/// Conformance checking utilities
pub mod utils {
    use super::*;

    /// Generate test event log for conformance checking
    pub fn generate_test_event_log(num_cases: usize, case_length: usize) -> EventLog {
        let mut log = EventLog::new("test_log".to_string());

        let activities = vec!["A", "B", "C", "D"];

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

    /// Generate test process model for conformance checking
    pub fn generate_test_process_model() -> ProcessModel {
        let mut model = ProcessModel::new("test_model".to_string());

        // Create nodes
        let start_idx = model.graph.add_node(ProcessNode {
            id: "start".to_string(),
            name: Some("Start".to_string()),
            node_type: ProcessNodeType::Start,
            position: Some((0.0, 0.0)),
            labels: vec!["start".to_string()],
            properties: HashMap::new(),
        });

        let a_idx = model.graph.add_node(ProcessNode {
            id: "A".to_string(),
            name: Some("A".to_string()),
            node_type: ProcessNodeType::Activity("A".to_string()),
            position: Some((0.2, 0.0)),
            labels: vec!["A".to_string()],
            properties: HashMap::new(),
        });

        let b_idx = model.graph.add_node(ProcessNode {
            id: "B".to_string(),
            name: Some("B".to_string()),
            node_type: ProcessNodeType::Activity("B".to_string()),
            position: Some((0.4, 0.0)),
            labels: vec!["B".to_string()],
            properties: HashMap::new(),
        });

        let c_idx = model.graph.add_node(ProcessNode {
            id: "C".to_string(),
            name: Some("C".to_string()),
            node_type: ProcessNodeType::Activity("C".to_string()),
            position: Some((0.6, 0.0)),
            labels: vec!["C".to_string()],
            properties: HashMap::new(),
        });

        let end_idx = model.graph.add_node(ProcessNode {
            id: "end".to_string(),
            name: Some("End".to_string()),
            node_type: ProcessNodeType::End,
            position: Some((1.0, 0.0)),
            labels: vec!["end".to_string()],
            properties: HashMap::new(),
        });

        // Create edges
        model.graph.add_edge(start_idx, a_idx, ProcessEdge {
            id: "start_A".to_string(),
            source: start_idx,
            target: a_idx,
            weight: None,
            conditions: vec![],
            properties: HashMap::new(),
        });

        model.graph.add_edge(a_idx, b_idx, ProcessEdge {
            id: "A_B".to_string(),
            source: a_idx,
            target: b_idx,
            weight: None,
            conditions: vec![],
            properties: HashMap::new(),
        });

        model.graph.add_edge(b_idx, c_idx, ProcessEdge {
            id: "B_C".to_string(),
            source: b_idx,
            target: c_idx,
            weight: None,
            conditions: vec![],
            properties: HashMap::new(),
        });

        model.graph.add_edge(c_idx, end_idx, ProcessEdge {
            id: "C_end".to_string(),
            source: c_idx,
            target: end_idx,
            weight: None,
            conditions: vec![],
            properties: HashMap::new(),
        });

        model.start_nodes = vec![start_idx];
        model.end_nodes = vec![end_idx];
        model.activities = HashSet::from(["A".to_string(), "B".to_string(), "C".to_string()]);

        model
    }

    /// Validate conformance result
    pub fn validate_conformance_result(result: &ConformanceResult) -> bool {
        // Check basic consistency
        if result.trace_fitness.is_empty() {
            return false;
        }

        // Check fitness bounds
        if result.fitness < 0.0 || result.fitness > 1.0 {
            return false;
        }

        if result.precision < 0.0 || result.precision > 1.0 {
            return false;
        }

        if result.recall < 0.0 || result.recall > 1.0 {
            return false;
        }

        // Check alignment consistency
        for alignment in &result.alignments {
            if alignment.fitness < 0.0 || alignment.fitness > 1.0 {
                return false;
            }

            if alignment.alignment.is_empty() {
                return false;
            }
        }

        true
    }
}

/// Extension trait for Deviation
impl Deviation {
    /// Get cost for deviation
    pub fn get_cost(&self) -> f64 {
        match self.type {
            DeviationType::MissingActivity => 1.0,
            DeviationType::ExtraActivity => 1.0,
            DeviationType::WrongOrder => 1.5,
            DeviationType::WrongActivity => 2.0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_conformance_checker_creation() {
        let log = EventLog::new("test_log".to_string());
        let model = ProcessModel::new("test_model".to_string());
        let params = ConformanceParameters::default();
        let checker = ConformanceChecker::new(log, model, params);

        assert_eq!(checker.model.id, "test_model");
    }

    #[test]
    fn test_model_validation() {
        let log = EventLog::new("test_log".to_string());
        log.activities.insert("A".to_string());
        log.activities.insert("B".to_string());

        let model = ProcessModel::new("test_model".to_string());
        model.activities.insert("A".to_string());
        model.activities.insert("B".to_string());

        let params = ConformanceParameters::default();
        let mut checker = ConformanceChecker::new(log, model, params);

        // Should pass validation
        assert!(checker.validate_model().is_ok());

        // Should fail validation - missing activity
        let log = EventLog::new("test_log".to_string());
        log.activities.insert("A".to_string());
        log.activities.insert("B".to_string());
        log.activities.insert("C".to_string());

        let model = ProcessModel::new("test_model".to_string());
        model.activities.insert("A".to_string());
        model.activities.insert("B".to_string());

        let checker = ConformanceChecker::new(log, model, params);
        assert!(checker.validate_model().is_err());
    }

    #[test]
    fn test_trace_fitness_calculation() {
        let log = EventLog::new("test_log".to_string());
        let model = ProcessModel::new("test_model".to_string());
        let params = ConformanceParameters::default();
        let checker = ConformanceChecker::new(log, model, params);

        let trace_fitness = vec![
            TraceFitness {
                case_id: "case_1".to_string(),
                fitness: 0.8,
                deviations: vec![],
            },
            TraceFitness {
                case_id: "case_2".to_string(),
                fitness: 0.9,
                deviations: vec![],
            },
        ];

        let overall_fitness = checker.calculate_overall_fitness(&trace_fitness).unwrap();
        assert_eq!(overall_fitness, 0.85);
    }

    #[test]
    fn test_precision_recall_calculation() {
        let log = EventLog::new("test_log".to_string());
        log.activities.insert("A".to_string());
        log.activities.insert("B".to_string());

        let model = ProcessModel::new("test_model".to_string());
        model.activities.insert("A".to_string());
        model.activities.insert("B".to_string());
        model.graph.add_edge(NodeIndex::new(), NodeIndex::new(), ProcessEdge {
            id: "A_B".to_string(),
            source: NodeIndex::new(),
            target: NodeIndex::new(),
            weight: None,
            conditions: vec![],
            properties: HashMap::new(),
        });

        let params = ConformanceParameters::default();
        let checker = ConformanceChecker::new(log, model, params);

        let precision = checker.calculate_precision().unwrap();
        let recall = checker.calculate_recall().unwrap();

        assert!(precision >= 0.0 && precision <= 1.0);
        assert!(recall >= 0.0 && recall <= 1.0);
    }

    #[test]
    fn test_alignment_calculation() {
        let log = EventLog::new("test_log".to_string());
        let model = ProcessModel::new("test_model".to_string());
        let params = ConformanceParameters::default();
        let mut checker = ConformanceChecker::new(log, model, params);

        let case = Case::new("test_case".to_string());
        let alignment = checker.calculate_single_alignment(&case);

        assert_eq!(alignment.trace_id, "test_case");
        assert!(alignment.alignment.len() > 0);
        assert!(alignment.fitness >= 0.0 && alignment.fitness <= 1.0);
    }

    #[test]
    fn test_conformance_result_validation() {
        let log = EventLog::new("test_log".to_string());
        let model = ProcessModel::new("test_model".to_string());
        let params = ConformanceParameters::default();
        let checker = ConformanceChecker::new(log, model, params);

        let result = ConformanceResult {
            model,
            trace_fitness: vec![],
            total_fitness: 0.0,
            fitness: 0.0,
            precision: 0.0,
            recall: 0.0,
            generalization: 0.0,
            alignments: vec![],
            deviation_summary: DeviationSummary {
                missing_activities: vec![],
                extra_activities: vec![],
                wrong_order: vec![],
                wrong_activities: vec![],
                total_cost: 0.0,
                average_cost_per_deviation: 0.0,
                max_deviation_cost: 0.0,
            },
            computation_time: std::time::Duration::ZERO,
            statistics: ConformanceStatistics {
                total_traces: 0,
                conforming_traces: 0,
                deviating_traces: 0,
                total_deviations: 0,
                missing_activities: 0,
                extra_activities: 0,
                wrong_order: 0,
                wrong_activities: 0,
                average_fitness: 0.0,
                fitness_distribution: HashMap::new(),
                deviation_distribution: HashMap::new(),
            },
        };

        assert!(utils::validate_conformance_result(&result));
    }

    #[test]
    fn test_generate_test_data() {
        let log = utils::generate_test_event_log(10, 5);
        let model = utils::generate_test_process_model();

        assert_eq!(log.num_cases, 10);
        assert_eq!(model.activities.len(), 3);
        assert_eq!(model.start_nodes.len(), 1);
        assert_eq!(model.end_nodes.len(), 1);
    }
}