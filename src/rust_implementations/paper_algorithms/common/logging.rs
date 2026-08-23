//! Logging utilities for process mining algorithms

use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use chrono::{DateTime, Utc};
use serde::{Serialize, Deserialize};
use tracing::{Level, Span};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

/// Process mining trace levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProcessMiningTraceLevel {
    /// Basic logging (errors and warnings)
    Basic,
    /// Detailed logging (all operations)
    Detailed,
    /// Verbose logging (debug information)
    Verbose,
    /// Debug logging (performance metrics)
    Debug,
    /// Ultra-verbose logging (memory dumps, etc.)
    Trace,
}

/// Log entry structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEntry {
    pub timestamp: DateTime<Utc>,
    pub level: LogLevel,
    pub module: String,
    pub message: String,
    pub algorithm: Option<String>,
    pub case_id: Option<String>,
    pub performance_metrics: Option<PerformanceMetrics>,
    pub error_details: Option<String>,
    pub context: Option<String>,
}

/// Log levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warning,
    Error,
    Critical,
}

impl From<Level> for LogLevel {
    fn from(level: Level) -> Self {
        match level {
            Level::TRACE => LogLevel::Trace,
            Level::DEBUG => LogLevel::Debug,
            Level::INFO => LogLevel::Info,
            Level::WARN => LogLevel::Warning,
            Level::ERROR => LogLevel::Error,
        }
    }
}

/// Logger configuration
#[derive(Debug, Clone)]
pub struct LoggerConfig {
    pub level: ProcessMiningTraceLevel,
    pub output: LogOutput,
    pub file_path: Option<PathBuf>,
    pub enable_performance_metrics: bool,
    pub enable_context_tracking: bool,
    pub enable_error_details: bool,
    pub max_file_size_mb: u64,
    pub max_files: usize,
    pub enable_color: bool,
    pub enable_json: bool,
}

/// Log output destinations
#[derive(Debug, Clone)]
pub enum LogOutput {
    Console,
    File,
    Both,
    None,
}

impl Default for LoggerConfig {
    fn default() -> Self {
        Self {
            level: ProcessMiningTraceLevel::Detailed,
            output: LogOutput::Both,
            file_path: Some(PathBuf::from("logs/process_mining.log")),
            enable_performance_metrics: true,
            enable_context_tracking: true,
            enable_error_details: true,
            max_file_size_mb: 10,
            max_files: 5,
            enable_color: true,
            enable_json: false,
        }
    }
}

/// Process mining logger
pub struct ProcessMiningLogger {
    config: LoggerConfig,
    file_handler: Option<Arc<Mutex<File>>>,
    performance_metrics: PerformanceMetricsTracker,
}

/// Performance metrics tracker
#[derive(Debug, Clone)]
pub struct PerformanceMetricsTracker {
    pub start_time: DateTime<Utc>,
    pub operation_counts: std::collections::HashMap<String, u64>,
    pub operation_durations: std::collections::HashMap<String, std::time::Duration>,
    pub memory_usage: Vec<u64>,
    pub active_operations: Vec<String>,
}

impl Default for PerformanceMetricsTracker {
    fn default() -> Self {
        Self {
            start_time: Utc::now(),
            operation_counts: std::collections::HashMap::new(),
            operation_durations: std::collections::HashMap::new(),
            memory_usage: Vec::new(),
            active_operations: Vec::new(),
        }
    }
}

impl PerformanceMetricsTracker {
    pub fn start_operation(&mut self, operation: &str) {
        self.active_operations.push(operation.to_string());
    }

    pub fn end_operation(&mut self, operation: &str, duration: std::time::Duration) {
        if let Some(index) = self.active_operations.iter().position(|s| s == operation) {
            self.active_operations.remove(index);
        }

        *self.operation_counts.entry(operation.to_string()).or_insert(0) += 1;
        let current_duration = self.operation_durations.entry(operation.to_string()).or_insert_default();
        *current_duration += duration;
    }

    pub fn log_memory_usage(&mut self) {
        self.memory_usage.push(self.get_current_memory_mb());
    }

    fn get_current_memory_mb(&self) -> u64 {
        // Simplified memory tracking - in production use proper memory monitoring
        let process = std::process::Command::new("ps")
            .args(&["-o", "rss=", "-p", &std::process::id().to_string()])
            .output();

        process
            .and_then(|output| {
                let output = String::from_utf8_lossy(&output.stdout);
                let mb = output.trim().parse::<u64>().unwrap_or(0) / 1024;
                Ok(mb)
            })
            .unwrap_or(0)
    }

    pub fn get_report(&self) -> String {
        let report = format!(
            "Performance Metrics Report\n\
            ==========================\n\
            Start Time: {}\n\
            Active Operations: {}\n\
            \n\
            Operation Statistics:\n",
            self.start_time,
            self.active_operations.len()
        );

        let mut operations: Vec<_> = self.operation_counts.iter().collect();
        operations.sort_by(|a, b| b.1.cmp(a.1));

        let operations_report = operations.iter()
            .map(|(op, count)| {
                let duration = self.operation_durations.get(op).unwrap_or(&std::time::Duration::ZERO);
                let avg_duration = duration.as_secs_f64() / *count as f64;
                format!(
                    "  {}: {} calls, {:.2}s total, {:.3}s avg",
                    op, count, duration.as_secs_f64(), avg_duration
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}\n{}", report, operations_report)
    }
}

impl ProcessMiningLogger {
    pub fn new(config: LoggerConfig) -> ProcessMiningResult<Self> {
        let mut logger = Self {
            config,
            file_handler: None,
            performance_metrics: PerformanceMetricsTracker::default(),
        };

        // Initialize file handler if needed
        if matches!(logger.config.output, LogOutput::File | LogOutput::Both) {
            logger.initialize_file_handler()?;
        }

        Ok(logger)
    }

    pub fn with_level(mut self, level: ProcessMiningTraceLevel) -> Self {
        self.config.level = level;
        self
    }

    pub fn with_file_path(mut self, path: PathBuf) -> Self {
        self.config.file_path = Some(path);
        self
    }

    fn initialize_file_handler(&mut self) -> ProcessMiningResult<()> {
        if let Some(path) = &self.config.file_path {
            // Ensure directory exists
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|e| ProcessMiningError::IoError(format!("Failed to create log directory: {}", e)))?;
            }

            // Open file
            let file = OpenOptions::new()
                .create(true)
                .append(true)
                .open(path)
                .map_err(|e| ProcessMiningError::IoError(format!("Failed to open log file: {}", e)))?;

            self.file_handler = Some(Arc::new(Mutex::new(file)));
        }
        Ok(())
    }

    pub fn log(&self, level: LogLevel, module: &str, message: &str) {
        let entry = LogEntry {
            timestamp: Utc::now(),
            level,
            module: module.to_string(),
            message: message.to_string(),
            algorithm: None,
            case_id: None,
            performance_metrics: None,
            error_details: None,
            context: None,
        };

        self.write_entry(entry);
    }

    pub fn log_with_context(
        &self,
        level: LogLevel,
        module: &str,
        message: &str,
        algorithm: Option<String>,
        case_id: Option<String>,
        context: Option<String>,
    ) {
        let entry = LogEntry {
            timestamp: Utc::now(),
            level,
            module: module.to_string(),
            message: message.to_string(),
            algorithm,
            case_id,
            performance_metrics: None,
            error_details: None,
            context,
        };

        self.write_entry(entry);
    }

    pub fn log_with_metrics(
        &self,
        level: LogLevel,
        module: &str,
        message: &str,
        algorithm: Option<String>,
        case_id: Option<String>,
        metrics: PerformanceMetrics,
    ) {
        let entry = LogEntry {
            timestamp: Utc::now(),
            level,
            module: module.to_string(),
            message: message.to_string(),
            algorithm,
            case_id,
            performance_metrics: Some(metrics),
            error_details: None,
            context: None,
        };

        self.write_entry(entry);
    }

    pub fn log_error(
        &self,
        module: &str,
        message: &str,
        error_details: Option<String>,
        algorithm: Option<String>,
        case_id: Option<String>,
        context: Option<String>,
    ) {
        let entry = LogEntry {
            timestamp: Utc::now(),
            level: LogLevel::Error,
            module: module.to_string(),
            message: message.to_string(),
            algorithm,
            case_id,
            performance_metrics: None,
            error_details,
            context,
        };

        self.write_entry(entry);
    }

    fn write_entry(&self, entry: LogEntry) {
        // Check log level
        if !self.should_log(entry.level) {
            return;
        }

        // Format log entry
        let formatted = self.format_entry(&entry);

        // Write to console
        if matches!(self.config.output, LogOutput::Console | LogOutput::Both) {
            eprintln!("{}", formatted);
        }

        // Write to file
        if matches!(self.config.output, LogOutput::File | LogOutput::Both) {
            if let Some(file_handler) = &self.file_handler {
                if let Ok(mut file) = file_handler.lock() {
                    writeln!(file, "{}", formatted).unwrap();
                }
            }
        }
    }

    fn should_log(&self, level: LogLevel) -> bool {
        let threshold = match self.config.level {
            ProcessMiningTraceLevel::Basic => LogLevel::Warning,
            ProcessMiningTraceLevel::Detailed => LogLevel::Debug,
            ProcessMiningTraceLevel::Verbose => LogLevel::Trace,
            ProcessMiningTraceLevel::Debug => LogLevel::Trace,
            ProcessMiningTraceLevel::Trace => LogLevel::Trace,
        };

        level >= threshold
    }

    fn format_entry(&self, entry: &LogEntry) -> String {
        let timestamp = entry.timestamp.format("%Y-%m-%dT%H:%M:%S%.3fZ");

        let level_color = match entry.level {
            LogLevel::Trace => colored::Color::White,
            LogLevel::Debug => colored::Color::Blue,
            LogLevel::Info => colored::Color::Green,
            LogLevel::Warning => colored::Color::Yellow,
            LogLevel::Error => colored::Color::Red,
            LogLevel::Critical => colored::Color::Magenta,
        };

        let level_str = format!("{:?}", entry.level).to_lowercase();
        let colored_level = colored::Colorize::colorize(&level_str, level_color);

        let base = format!(
            "{} [{}] {}: {}",
            timestamp,
            colored_level,
            entry.module,
            entry.message
        );

        if self.config.enable_context {
            if let Some(algorithm) = &entry.algorithm {
                base.push_str(&format!(" [Algorithm: {}]", algorithm));
            }
            if let Some(case_id) = &entry.case_id {
                base.push_str(&format!(" [Case: {}]", case_id));
            }
            if let Some(context) = &entry.context {
                base.push_str(&format!(" [Context: {}]", context));
            }
        }

        if self.config.enable_error_details {
            if let Some(error_details) = &entry.error_details {
                base.push_str(&format!(" [Error: {}]", error_details));
            }
        }

        if self.config.enable_performance_metrics {
            if let Some(metrics) = &entry.performance_metrics {
                base.push_str(&format!(" [Metrics: {}]", metrics));
            }
        }

        if self.config.enable_json {
            serde_json::to_string(entry).unwrap_or_else(|_| base)
        } else {
            base
        }
    }

    pub fn start_operation(&mut self, operation: &str) {
        self.performance_metrics.start_operation(operation);
    }

    pub fn end_operation(&mut self, operation: &str, duration: std::time::Duration) {
        self.performance_metrics.end_operation(operation, duration);
    }

    pub fn get_performance_report(&self) -> String {
        self.performance_metrics.get_report()
    }

    pub fn log_memory_usage(&mut self) {
        self.performance_metrics.log_memory_usage();
    }

    pub fn get_current_memory_mb(&u64) {
        self.performance_metrics.get_current_memory_mb();
    }
}

/// Global logger instance
pub static mut GLOBAL_LOGGER: Option<ProcessMiningLogger> = None;
pub static LOGGER_MUTEX: std::sync::Once = std::sync::Once::new();

/// Initialize global logger
pub fn setup_tracing(level: ProcessMiningTraceLevel) {
    LOGGER_MUTEX.call_once(|| {
        let config = LoggerConfig::default().with_level(level);
        unsafe {
            GLOBAL_LOGGER = Some(ProcessMiningLogger::new(config).unwrap());
        }

        tracing_subscriber::registry()
            .with(
                tracing_subscriber::fmt::layer()
                    .compact()
                    .with_timer(tracing_subscriber::fmt::time::ChronoLocal::rfc_3339())
                    .with_target(false)
                    .with_thread_names(true)
                    .with_thread_ids(true)
            )
            .init();
    });
}

/// Global log macros
#[macro_export]
macro_rules! trace_pm {
    ($module:expr, $($arg:tt)*) => {
        if let Some(logger) = unsafe { &GLOBAL_LOGGER } {
            logger.log(LogLevel::Trace, $module, &format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! debug_pm {
    ($module:expr, $($arg:tt)*) => {
        if let Some(logger) = unsafe { &GLOBAL_LOGGER } {
            logger.log(LogLevel::Debug, $module, &format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! info_pm {
    ($module:expr, $($arg:tt)*) => {
        if let Some(logger) = unsafe { &GLOBAL_LOGGER } {
            logger.log(LogLevel::Info, $module, &format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! warn_pm {
    ($module:expr, $($arg:tt)*) => {
        if let Some(logger) = unsafe { &GLOBAL_LOGGER } {
            logger.log(LogLevel::Warning, $module, &format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! error_pm {
    ($module:expr, $($arg:tt)*) => {
        if let Some(logger) = unsafe { &GLOBAL_LOGGER } {
            logger.log(LogLevel::Error, $module, &format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_operation {
    ($operation:expr, $logger:expr, $($arg:tt)*) => {{
        let start = std::time::Instant::now();
        debug_pm!($logger, "Starting operation: {}", $operation);
        let result = (|| { $($arg)* })();
        let duration = start.elapsed();
        if let Err(ref e) = &result {
            error_pm!($logger, "Operation {} failed: {:?}", $operation, e);
        } else {
            debug_pm!($logger, "Operation {} completed in {:?}", $operation, duration);
        }
        $logger.end_operation($operation, duration);
        result
    }};
}

/// Performance timing utility
pub struct OperationTimer {
    operation: String,
    start: std::time::Instant,
    logger: Option<ProcessMiningLogger>,
}

impl OperationTimer {
    pub fn new(operation: &str, logger: Option<ProcessMiningLogger>) -> Self {
        Self {
            operation: operation.to_string(),
            start: std::time::Instant::now(),
            logger,
        }
    }

    pub fn with_logger(operation: &str, logger: &ProcessMiningLogger) -> Self {
        Self {
            operation: operation.to_string(),
            start: std::time::Instant::now(),
            logger: Some(logger.clone()),
        }
    }

    pub fn time<T, F>(operation: &str, f: F) -> (T, std::time::Duration)
    where
        F: FnOnce() -> T,
    {
        let start = std::time::Instant::now();
        let result = f();
        let duration = start.elapsed();
        (result, duration)
    }
}

impl Drop for OperationTimer {
    fn drop(&mut self) {
        let duration = self.start.elapsed();

        if let Some(logger) = &self.logger {
            logger.end_operation(&self.operation, duration);
        }

        if duration.as_secs_f64() > 1.0 {
            warn_pm!("timing", "Slow operation: {} took {:?}", self.operation, duration);
        }
    }
}

/// Thread-safe log entry collector
pub struct LogCollector {
    entries: Arc<Mutex<Vec<LogEntry>>>,
}

impl LogCollector {
    pub fn new() -> Self {
        Self {
            entries: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn log(&self, entry: LogEntry) {
        if let Ok(mut entries) = self.entries.lock() {
            entries.push(entry);
        }
    }

    pub fn get_entries(&self) -> ProcessMiningResult<Vec<LogEntry>> {
        let entries = self.entries.lock()
            .map_err(|e| ProcessMiningError::IoError(format!("Failed to lock log entries: {}", e)))?;
        Ok(entries.clone())
    }

    pub fn clear(&self) {
        if let Ok(mut entries) = self.entries.lock() {
            entries.clear();
        }
    }
}

/// Testing utilities
pub mod test_utils {
    use super::*;

    pub fn setup_test_logger() -> ProcessMiningLogger {
        let config = LoggerConfig::default()
            .with_level(ProcessMiningTraceLevel::Debug)
            .with_file_path(std::path::PathBuf::from("logs/test.log"));

        ProcessMiningLogger::new(config).unwrap()
    }

    pub fn cleanup_test_logs() {
        if std::path::Path::new("logs/test.log").exists() {
            std::fs::remove_file("logs/test.log").unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_logger_creation() {
        let logger = ProcessMiningLogger::new(LoggerConfig::default()).unwrap();
        assert!(matches!(logger.config.level, ProcessMiningTraceLevel::Detailed));
    }

    #[test]
    fn test_log_level_conversion() {
        let rust_level = Level::DEBUG;
        let pm_level: LogLevel = rust_level.into();
        assert_eq!(pm_level, LogLevel::Debug);
    }

    #[test]
    fn test_operation_timer() {
        let (result, duration) = OperationTimer::time("test", || {
            std::thread::sleep(std::time::Duration::from_millis(10));
            42
        });
        assert_eq!(result, 42);
        assert!(duration.as_millis() >= 10);
    }

    #[test]
    fn test_log_collector() {
        let collector = LogCollector::new();
        let entry = LogEntry {
            timestamp: Utc::now(),
            level: LogLevel::Info,
            module: "test".to_string(),
            message: "Test message".to_string(),
            algorithm: None,
            case_id: None,
            performance_metrics: None,
            error_details: None,
            context: None,
        };

        collector.log(entry.clone());

        let entries = collector.get_entries().unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].message, "Test message");
    }

    #[test]
    fn test_performance_metrics() {
        let mut tracker = PerformanceMetricsTracker::default();

        tracker.start_operation("test_op");
        std::thread::sleep(std::time::Duration::from_millis(10));
        tracker.end_operation("test_op", std::time::Duration::from_millis(10));

        assert_eq!(tracker.operation_counts["test_op"], 1);
        assert!(tracker.operation_durations["test_op"] >= std::time::Duration::from_millis(10));
    }
}