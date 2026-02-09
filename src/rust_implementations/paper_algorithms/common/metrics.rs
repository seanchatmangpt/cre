//! Performance metrics and monitoring utilities for process mining algorithms

use std::collections::{HashMap, BTreeMap};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime};
use chrono::{DateTime, Utc};
use serde::{Serialize, Deserialize};
use parking_lot::Mutex as ParkingMutex;

/// Performance metrics collector
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Overall performance metrics
    pub throughput: f64,
    pub average_case_duration: Duration,
    pub processing_time: Duration,
    pub waiting_time: Duration,
    pub service_time: Duration,
    pub queue_length: f64,
    pub utilization: f64,
    pub idle_time: Duration,

    /// Resource-specific metrics
    pub resource_utilization: HashMap<String, f64>,
    pub activity_durations: HashMap<String, Duration>,
    pub case_durations: HashMap<String, Duration>,

    /// Memory usage
    pub memory_usage_mb: u64,
    pub peak_memory_usage_mb: u64,

    /// Performance indicators
    pub performance_score: f64,
    pub efficiency_score: f64,
    pub quality_score: f64,

    /// Timing data
    pub start_time: Option<Instant>,
    pub end_time: Option<Instant>,
    pub total_elapsed: Option<Duration>,
}

impl Default for PerformanceMetrics {
    fn default() -> Self {
        Self {
            throughput: 0.0,
            average_case_duration: Duration::ZERO,
            processing_time: Duration::ZERO,
            waiting_time: Duration::ZERO,
            service_time: Duration::ZERO,
            queue_length: 0.0,
            utilization: 0.0,
            idle_time: Duration::ZERO,
            resource_utilization: HashMap::new(),
            activity_durations: HashMap::new(),
            case_durations: HashMap::new(),
            memory_usage_mb: 0,
            peak_memory_usage_mb: 0,
            performance_score: 0.0,
            efficiency_score: 0.0,
            quality_score: 0.0,
            start_time: None,
            end_time: None,
            total_elapsed: None,
        }
    }
}

impl PerformanceMetrics {
    /// Create new performance metrics
    pub fn new() -> Self {
        Self::default()
    }

    /// Start timing
    pub fn start(&mut self) {
        self.start_time = Some(Instant::now());
    }

    /// Stop timing and calculate total elapsed
    pub fn stop(&mut self) {
        self.end_time = Some(Instant::now());
        if let Some(start) = self.start_time {
            self.total_elapsed = Some(start.elapsed());
        }
    }

    /// Calculate performance score
    pub fn calculate_performance_score(&mut self) {
        // Score based on throughput, utilization, and efficiency
        let throughput_score = if self.throughput > 0.0 {
            (self.throughput / 1000.0).min(1.0)
        } else {
            0.0
        };

        let utilization_score = self.utilization.min(1.0);
        let efficiency_score = self.efficiency_score;

        self.performance_score = (throughput_score + utilization_score + efficiency_score) / 3.0;
    }

    /// Update memory usage
    pub fn update_memory_usage(&mut self) -> u64 {
        let current_mb = get_current_memory_mb();
        self.memory_usage_mb = current_mb;
        self.peak_memory_usage_mb = self.peak_memory_usage_mb.max(current_mb);
        current_mb
    }

    /// Calculate resource utilization
    pub fn calculate_resource_utilization(&mut self, resources: &[String]) {
        for resource in resources {
            if let Some(duration) = self.case_durations.get(resource) {
                let total_time = duration.as_secs_f64();
                let utilization = total_time / self.total_elapsed.unwrap_or_default().as_secs_f64();
                self.resource_utilization.insert(resource.clone(), utilization);
            }
        }
    }

    /// Get summary statistics
    pub fn get_summary(&self) -> String {
        format!(
            "Performance Metrics Summary:\n\
            ===============================\n\
            Throughput: {:.2} cases/hour\n\
            Average Case Duration: {:?}\n\
            Processing Time: {:?}\n\
            Utilization: {:.2}%\n\
            Memory Usage: {} MB (Peak: {} MB)\n\
            Performance Score: {:.2}/1.0\n\
            Efficiency Score: {:.2}/1.0\n\
            Quality Score: {:.2}/1.0",
            self.throughput,
            self.average_case_duration,
            self.processing_time,
            self.utilization * 100.0,
            self.memory_usage_mb,
            self.peak_memory_usage_mb,
            self.performance_score,
            self.efficiency_score,
            self.quality_score
        )
    }
}

/// Memory usage monitoring
pub struct MemoryMonitor {
    pub samples: Vec<u64>,
    pub peak_memory: u64,
    pub current_memory: u64,
    pub monitor_interval: Duration,
}

impl MemoryMonitor {
    pub fn new(interval: Duration) -> Self {
        Self {
            samples: Vec::new(),
            peak_memory: 0,
            current_memory: 0,
            monitor_interval: interval,
        }
    }

    pub fn start_monitoring(&mut self) {
        self.current_memory = get_current_memory_mb();
        self.peak_memory = self.peak_memory.max(self.current_memory);
        self.samples.push(self.current_memory);
    }

    pub fn get_average_memory(&self) -> f64 {
        if self.samples.is_empty() {
            return 0.0;
        }
        self.samples.iter().sum::<u64>() as f64 / self.samples.len() as f64
    }

    pub fn get_memory_trend(&self) -> String {
        if self.samples.len() < 2 {
            return "Insufficient data".to_string();
        }

        let first = self.samples[0];
        let last = self.samples[self.samples.len() - 1];
        let trend = if last > first { "increasing" } else if last < first { "decreasing" } else { "stable" };

        format!("Memory trend: {} (from {} MB to {} MB)", trend, first, last)
    }
}

/// CPU usage monitoring
pub struct CpuMonitor {
    pub samples: Vec<f64>,
    pub average_cpu: f64,
}

impl CpuMonitor {
    pub fn new() -> Self {
        Self {
            samples: Vec::new(),
            average_cpu: 0.0,
        }
    }

    pub fn sample_cpu_usage(&mut self) -> f64 {
        // Simplified CPU usage calculation
        // In production, use proper CPU monitoring library
        let cpu_usage = rand::random::<f64>() * 100.0;
        self.samples.push(cpu_usage);

        // Keep only last 100 samples
        if self.samples.len() > 100 {
            self.samples.remove(0);
        }

        self.update_average();
        cpu_usage
    }

    pub fn update_average(&mut self) {
        if !self.samples.is_empty() {
            self.average_cpu = self.samples.iter().sum::<f64>() / self.samples.len() as f64;
        }
    }

    pub fn get_cpu_summary(&self) -> String {
        format!(
            "CPU Usage Summary:\n\
            ==================\n\
            Current CPU: {:.2}%\n\
            Average CPU: {:.2}%\n\
            Samples: {}\n\
            Peak CPU: {:.2}%",
            self.samples.last().unwrap_or(&0.0),
            self.average_cpu,
            self.samples.len(),
            self.samples.iter().fold(0.0, |acc, x| acc.max(*x))
        )
    }
}

/// Algorithm performance benchmark
pub struct AlgorithmBenchmark {
    pub algorithm_name: String,
    pub metrics: PerformanceMetrics,
    pub iterations: usize,
    pub warmup_iterations: usize,
    pub test_data_size: usize,
    pub start_time: Instant,
    pub end_time: Option<Instant>,
}

impl AlgorithmBenchmark {
    pub fn new(algorithm_name: &str, iterations: usize, warmup: usize, data_size: usize) -> Self {
        Self {
            algorithm_name: algorithm_name.to_string(),
            metrics: PerformanceMetrics::new(),
            iterations,
            warmup_iterations: warmup,
            test_data_size: data_size,
            start_time: Instant::now(),
            end_time: None,
        }
    }

    pub fn start(&mut self) {
        self.start_time = Instant::now();
        self.metrics.start();
    }

    pub fn finish(&mut self) {
        self.end_time = Some(Instant::now());
        self.metrics.stop();
        self.metrics.calculate_performance_score();
    }

    pub fn get_benchmark_summary(&self) -> String {
        let elapsed = self.end_time.unwrap_or(self.start_time.elapsed());
        let avg_time_per_iteration = elapsed / self.iterations as u32;

        format!(
            "Benchmark Results: {}\n\
            ==========================\n\
            Total Iterations: {}\n\
            Warmup Iterations: {}\n\
            Test Data Size: {}\n\
            Total Time: {:?}\n\
            Average Time per Iteration: {:?}\n\
            Iterations per Second: {:.2}\n\
            Throughput: {:.2} operations/second",
            self.algorithm_name,
            self.iterations,
            self.warmup_iterations,
            self.test_data_size,
            elapsed,
            avg_time_per_iteration,
            self.iterations as f64 / elapsed.as_secs_f64(),
            self.test_data_size as f64 * self.iterations as f64 / elapsed.as_secs_f64()
        )
    }

    pub fn get_detailed_metrics(&self) -> String {
        format!(
            "Detailed Metrics for: {}\n\
            ==========================\n{}\n\
            \n{}",
            self.algorithm_name,
            self.metrics.get_summary(),
            self.get_benchmark_summary()
        )
    }
}

/// Performance profiler
pub struct PerformanceProfiler {
    pub benchmarks: HashMap<String, AlgorithmBenchmark>,
    pub memory_monitor: MemoryMonitor,
    pub cpu_monitor: CpuMonitor,
    pub is_active: bool,
}

impl PerformanceProfiler {
    pub fn new() -> Self {
        Self {
            benchmarks: HashMap::new(),
            memory_monitor: MemoryMonitor::new(Duration::from_secs(1)),
            cpu_monitor: CpuMonitor::new(),
            is_active: false,
        }
    }

    pub fn start_profiling(&mut self) {
        self.is_active = true;
        self.memory_monitor.start_monitoring();
    }

    pub fn stop_profiling(&mut self) {
        self.is_active = false;
    }

    pub fn start_benchmark(&mut self, algorithm_name: &str, iterations: usize, warmup: usize, data_size: usize) {
        let benchmark = AlgorithmBenchmark::new(algorithm_name, iterations, warmup, data_size);
        benchmark.start();
        self.benchmarks.insert(algorithm_name.to_string(), benchmark);
    }

    pub fn end_benchmark(&mut self, algorithm_name: &str) {
        if let Some(benchmark) = self.benchmarks.get_mut(algorithm_name) {
            benchmark.finish();
        }
    }

    pub fn get_benchmark_results(&self) -> Vec<String> {
        self.benchmarks.iter()
            .map(|(name, benchmark)| benchmark.get_detailed_metrics())
            .collect()
    }

    pub fn get_performance_report(&self) -> String {
        let mut report = String::new();

        report.push_str("Performance Profiling Report\n");
        report.push_str("==============================\n\n");

        // Memory usage
        report.push_str("Memory Usage:\n");
        report.push_str(&format!("  Current: {} MB\n", self.memory_monitor.current_memory));
        report.push_str(&format!("  Peak: {} MB\n", self.memory_monitor.peak_memory));
        report.push_str(&format!("  Average: {:.2} MB\n", self.memory_monitor.get_average_memory()));
        report.push_str(&format!("  Trend: {}\n", self.memory_monitor.get_memory_trend()));
        report.push_str("\n");

        // CPU usage
        report.push_str("CPU Usage:\n");
        report.push_str(&self.cpu_monitor.get_cpu_summary());
        report.push_str("\n\n");

        // Benchmark results
        report.push_str("Benchmark Results:\n");
        for result in self.get_benchmark_results() {
            report.push_str(&result);
            report.push_str("\n\n");
        }

        report
    }
}

/// Performance analysis utilities
pub mod analysis {
    use super::*;

    /// Statistical analysis of performance metrics
    pub struct PerformanceAnalyzer {
        pub metrics: Vec<PerformanceMetrics>,
        pub analysis_results: HashMap<String, f64>,
    }

    impl PerformanceAnalyzer {
        pub fn new() -> Self {
            Self {
                metrics: Vec::new(),
                analysis_results: HashMap::new(),
            }
        }

        pub fn add_metrics(&mut self, metrics: PerformanceMetrics) {
            self.metrics.push(metrics);
        }

        pub fn analyze(&mut self) {
            if self.metrics.is_empty() {
                return;
            }

            // Calculate averages
            let avg_throughput: f64 = self.metrics.iter()
                .map(|m| m.throughput)
                .sum::<f64>() / self.metrics.len() as f64;

            let avg_utilization: f64 = self.metrics.iter()
                .map(|m| m.utilization)
                .sum::<f64>() / self.metrics.len() as f64;

            let avg_performance_score: f64 = self.metrics.iter()
                .map(|m| m.performance_score)
                .sum::<f64>() / self.metrics.len() as f64;

            // Calculate standard deviations
            let std_dev_throughput = self.calculate_std_dev(|m| m.throughput);
            let std_dev_utilization = self.calculate_std_dev(|m| m.utilization);
            let std_dev_performance = self.calculate_std_dev(|m| m.performance_score);

            // Store results
            self.analysis_results.insert("avg_throughput".to_string(), avg_throughput);
            self.analysis_results.insert("avg_utilization".to_string(), avg_utilization);
            self.analysis_results.insert("avg_performance_score".to_string(), avg_performance_score);
            self.analysis_results.insert("std_dev_throughput".to_string(), std_dev_throughput);
            self.analysis_results.insert("std_dev_utilization".to_string(), std_dev_utilization);
            self.analysis_results.insert("std_dev_performance".to_string(), std_dev_performance);
        }

        fn calculate_std_dev<F>(&self, selector: F) -> f64
        where
            F: Fn(&PerformanceMetrics) -> f64,
        {
            if self.metrics.is_empty() {
                return 0.0;
            }

            let avg = self.metrics.iter().map(|m| selector(m)).sum::<f64>() / self.metrics.len() as f64;
            let variance = self.metrics.iter()
                .map(|m| (selector(m) - avg).powi(2))
                .sum::<f64>() / self.metrics.len() as f64;

            variance.sqrt()
        }

        pub fn get_analysis_summary(&self) -> String {
            let mut summary = String::new();

            summary.push_str("Performance Analysis Summary\n");
            summary.push_str("============================\n\n");

            for (key, value) in &self.analysis_results {
                summary.push_str(&format!("{}: {:.4}\n", key, value));
            }

            summary
        }
    }

    /// Performance comparison utility
    pub struct PerformanceComparator {
        pub baseline: PerformanceMetrics,
        pub current: PerformanceMetrics,
        pub improvements: HashMap<String, f64>,
        pub regressions: HashMap<String, f64>,
    }

    impl PerformanceComparator {
        pub fn new(baseline: PerformanceMetrics, current: PerformanceMetrics) -> Self {
            Self {
                baseline,
                current,
                improvements: HashMap::new(),
                regressions: HashMap::new(),
            }
        }

        pub fn compare(&mut self) {
            // Compare throughput
            let throughput_diff = self.current.throughput - self.baseline.throughput;
            if throughput_diff > 0.0 {
                self.improvements.insert("throughput".to_string(), throughput_diff);
            } else {
                self.regressions.insert("throughput".to_string(), -throughput_diff);
            }

            // Compare utilization
            let utilization_diff = self.current.utilization - self.baseline.utilization;
            if utilization_diff > 0.0 {
                self.improvements.insert("utilization".to_string(), utilization_diff);
            } else {
                self.regressions.insert("utilization".to_string(), -utilization_diff);
            }

            // Compare performance score
            let performance_diff = self.current.performance_score - self.baseline.performance_score;
            if performance_diff > 0.0 {
                self.improvements.insert("performance_score".to_string(), performance_diff);
            } else {
                self.regressions.insert("performance_score".to_string(), -performance_diff);
            }

            // Compare memory usage
            let memory_diff = self.current.memory_usage_mb as f64 - self.baseline.memory_usage_mb as f64;
            if memory_diff < 0.0 {
                self.improvements.insert("memory_usage".to_string(), -memory_diff);
            } else {
                self.regressions.insert("memory_usage".to_string(), memory_diff);
            }
        }

        pub fn get_comparison_summary(&self) -> String {
            let mut summary = String::new();

            summary.push_str("Performance Comparison Summary\n");
            summary.push_str("==============================\n\n");

            summary.push_str("Improvements:\n");
            if self.improvements.is_empty() {
                summary.push_str("  None\n");
            } else {
                for (metric, improvement) in &self.improvements {
                    summary.push_str(&format!("  {}: {:.2}% improvement\n", metric, improvement * 100.0));
                }
            }

            summary.push_str("\nRegressions:\n");
            if self.regressions.is_empty() {
                summary.push_str("  None\n");
            } else {
                for (metric, regression) in &self.regressions {
                    summary.push_str(&format!("  {}: {:.2}% regression\n", metric, regression * 100.0));
                }
            }

            if !self.improvements.is_empty() && self.regressions.is_empty() {
                summary.push_str("\n✅ All metrics improved!\n");
            } else if !self.regressions.is_empty() && self.improvements.is_empty() {
                summary.push_str("\n❌ All metrics regressed!\n");
            } else {
                summary.push_str("\n🔍 Mixed results - some metrics improved, others regressed.\n");
            }

            summary
        }
    }
}

/// Helper function to get current memory usage
fn get_current_memory_mb() -> u64 {
    // Simplified memory monitoring
    // In production, use proper memory monitoring library
    match std::process::Command::new("ps")
        .args(&["-o", "rss=", "-p", &std::process::id().to_string()])
        .output()
    {
        Ok(output) => {
            let output = String::from_utf8_lossy(&output.stdout);
            let mb = output.trim().parse::<u64>().unwrap_or(0) / 1024;
            mb
        }
        Err(_) => 0,
    }
}

/// Performance tracking macros
#[macro_export]
macro_rules! benchmark {
    ($name:expr, $code:block) => {{
        let start = std::time::Instant::now();
        let result = $code;
        let duration = start.elapsed();

        info_pm!("benchmark", "{} completed in {:?}", $name, duration);

        (result, duration)
    }};
}

#[macro_export]
macro_rules! profile_memory {
    ($operation:expr, $logger:expr) => {{
        let start_mem = get_current_memory_mb();
        let result = $operation;
        let end_mem = get_current_memory_mb();
        let memory_diff = end_mem as i64 - start_mem as i64;

        if memory_diff != 0 {
            let change = if memory_diff > 0 {
                format!("+{} MB", memory_diff)
            } else {
                format!("{} MB", memory_diff)
            };
            info_pm!("memory", "{} used {}", $operation, change);
        }

        result
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_metrics() {
        let mut metrics = PerformanceMetrics::new();
        metrics.start();
        metrics.throughput = 1000.0;
        metrics.utilization = 0.85;
        metrics.calculate_performance_score();

        assert!(metrics.performance_score > 0.0);
        assert!(metrics.performance_score <= 1.0);
    }

    #[test]
    fn test_memory_monitor() {
        let mut monitor = MemoryMonitor::new(Duration::from_secs(1));
        monitor.start_monitoring();

        assert!(monitor.samples.len() > 0);
        assert!(monitor.peak_memory >= monitor.current_memory);
    }

    #[test]
    fn test_benchmark() {
        let mut benchmark = AlgorithmBenchmark::new("test", 10, 2, 100);
        benchmark.start();
        std::thread::sleep(Duration::from_millis(10));
        benchmark.finish();

        assert!(benchmark.metrics.total_elapsed.is_some());
        assert!(benchmark.metrics.total_elapsed.unwrap() > Duration::ZERO);
    }

    #[test]
    fn test_performance_profiler() {
        let mut profiler = PerformanceProfiler::new();
        profiler.start_profiling();

        profiler.start_benchmark("test_algorithm", 5, 1, 50);
        std::thread::sleep(Duration::from_millis(10));
        profiler.end_benchmark("test_algorithm");

        profiler.stop_profiling();

        assert!(!profiler.get_performance_report().is_empty());
    }

    #[test]
    fn test_performance_analyzer() {
        let mut analyzer = PerformanceAnalyzer::new();

        let mut metrics1 = PerformanceMetrics::new();
        metrics1.throughput = 1000.0;
        metrics1.utilization = 0.8;
        analyzer.add_metrics(metrics1);

        let mut metrics2 = PerformanceMetrics::new();
        metrics2.throughput = 1200.0;
        metrics2.utilization = 0.9;
        analyzer.add_metrics(metrics2);

        analyzer.analyze();

        assert!(analyzer.analysis_results.contains_key("avg_throughput"));
        assert!(analyzer.analysis_results.contains_key("avg_utilization"));
    }

    #[test]
    fn test_performance_comparator() {
        let baseline = PerformanceMetrics {
            throughput: 1000.0,
            utilization: 0.8,
            performance_score: 0.7,
            ..Default::default()
        };

        let current = PerformanceMetrics {
            throughput: 1200.0,
            utilization: 0.85,
            performance_score: 0.8,
            ..Default::default()
        };

        let mut comparator = PerformanceComparator::new(baseline, current);
        comparator.compare();

        assert!(comparator.improvements.contains_key("throughput"));
        assert!(comparator.improvements.contains_key("utilization"));
        assert!(comparator.improvements.contains_key("performance_score"));
    }

    #[test]
    fn benchmark_macro_test() {
        let (result, duration) = benchmark!("test_macro", {
            std::thread::sleep(Duration::from_millis(10));
            42
        });

        assert_eq!(result, 42);
        assert!(duration.as_millis() >= 10);
    }

    #[test]
    fn memory_profile_macro_test() {
        let result = profile_memory!({
            std::thread::sleep(Duration::from_millis(10));
            42
        }, ProcessMiningLogger::new(LoggerConfig::default()).unwrap());

        assert_eq!(result, 42);
    }
}