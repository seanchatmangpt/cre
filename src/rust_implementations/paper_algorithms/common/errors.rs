//! Error handling and custom error types for process mining algorithms

use std::fmt;
use serde::{Serialize, Deserialize};

/// Core process mining error types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ProcessMiningError {
    /// Invalid event log format or structure
    InvalidEventLog(String),

    /// Case not found in event log
    CaseNotFound(String),

    /// Duplicate case ID found
    DuplicateCaseId(String),

    /// Invalid process model structure
    InvalidModel(String),

    /// Computation error (mathematical, overflow, etc.)
    ComputationError(String),

    /// Parse error (XES, JSON, etc.)
    ParseError(String),

    /// Validation error (business rules, constraints)
    ValidationError(String),

    /// I/O error (file operations, network)
    IoError(String),

    /// Timeout error (computation took too long)
    TimeoutError(String),

    /// Memory error (out of memory)
    MemoryError(String),

    /// Configuration error
    ConfigurationError(String),

    /// Algorithm specific error
    AlgorithmError(String),

    /// Data format error
    FormatError(String),

    /// Statistical error
    StatisticalError(String),

    /// Resource not found
    ResourceNotFound(String),

    /// Permission error
    PermissionError(String),

    /// System error
    SystemError(String),
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
            ProcessMiningError::MemoryError(msg) => write!(f, "Memory error: {}", msg),
            ProcessMiningError::ConfigurationError(msg) => write!(f, "Configuration error: {}", msg),
            ProcessMiningError::AlgorithmError(msg) => write!(f, "Algorithm error: {}", msg),
            ProcessMiningError::FormatError(msg) => write!(f, "Format error: {}", msg),
            ProcessMiningError::StatisticalError(msg) => write!(f, "Statistical error: {}", msg),
            ProcessMiningError::ResourceNotFound(msg) => write!(f, "Resource not found: {}", msg),
            ProcessMiningError::PermissionError(msg) => write!(f, "Permission error: {}", msg),
            ProcessMiningError::SystemError(msg) => write!(f, "System error: {}", msg),
        }
    }
}

impl std::error::Error for ProcessMiningError {}

/// Result type for process mining operations
pub type ProcessMiningResult<T> = Result<T, ProcessMiningError>;

/// Error chain for composing errors
pub trait ErrorChain<T> {
    fn chain<U, F: FnOnce(T) -> ProcessMiningResult<U>>(self, f: F) -> ProcessMiningResult<U>;
}

impl<T, E: Into<ProcessMiningError>> ErrorChain<Result<T, E>> for Result<T, E> {
    fn chain<U, F: FnOnce(T) -> ProcessMiningResult<U>>(self, f: F) -> ProcessMiningResult<U> {
        match self {
            Ok(value) => f(value),
            Err(error) => Err(error.into()),
        }
    }
}

/// Macro for creating ProcessMiningError::ComputationError
#[macro_export]
macro_rules! computation_error {
    ($($arg:tt)*) => {
        ProcessMiningError::ComputationError(format!($($arg)*))
    };
}

/// Macro for creating ProcessMiningError::ValidationError
#[macro_export]
macro_rules! validation_error {
    ($($arg:tt)*) => {
        ProcessMiningError::ValidationError(format!($($arg)*))
    };
}

/// Macro for creating ProcessMiningError::AlgorithmError
#[macro_export]
macro_rules! algorithm_error {
    ($($arg:tt)*) => {
        ProcessMiningError::AlgorithmError(format!($($arg)*))
    };
}

/// Error context helper
pub struct ErrorContext<T> {
    value: T,
    context: String,
}

impl<T> ErrorContext<T> {
    pub fn new(value: T, context: impl Into<String>) -> Self {
        Self {
            value,
            context: context.into(),
        }
    }

    pub fn chain<F, U>(self, f: F) -> ProcessMiningResult<U>
    where
        F: FnOnce(T) -> ProcessMiningResult<U>,
    {
        match f(self.value) {
            Ok(value) => Ok(value),
            Err(error) => Err(
                ProcessMiningError::ComputationError(
                    format!("{}: {}", self.context, error)
                )
            ),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> ErrorContext<U> {
        ErrorContext::new(f(self.value), self.context)
    }
}

/// Custom result type for operations that might fail
pub type Result<T> = std::result::Result<T, ProcessMiningError>;

/// Assertion macro for process mining invariants
#[macro_export]
macro_rules! assert_pm {
    ($condition:expr, $($arg:tt)*) => {
        if !$condition {
            return Err(ProcessMiningError::ValidationError(format!($($arg)*)));
        }
    };
}

/// Safe wrapper for operations that might panic
pub struct SafeOperation<T> {
    pub result: Result<T>,
}

impl<T> SafeOperation<T> {
    pub fn new<F>(f: F, operation: &str) -> Self
    where
        F: FnOnce() -> T,
    {
        Self {
            result: std::panic::catch_unwind(|| f())
                .map_err(|_| {
                    ProcessMiningError::ComputationError(
                        format!("Panic in {}: operation failed", operation)
                    )
                })
                .map_err(|err| {
                    ProcessMiningError::ComputationError(
                        format!("{}: {}", operation, err)
                    )
                }),
        }
    }
}

/// Error recovery utilities
pub mod recovery {
    use super::*;

    /// Retry operation with exponential backoff
    pub fn retry_with_backoff<F, T>(
        mut f: F,
        max_retries: usize,
        base_delay_ms: u64,
    ) -> ProcessMiningResult<T>
    where
        F: FnMut() -> ProcessMiningResult<T>,
    {
        let mut delay = base_delay_ms;

        for attempt in 0..max_retries {
            match f() {
                Ok(result) => return Ok(result),
                Err(error) if attempt == max_retries - 1 => return Err(error),
                _ => {
                    std::thread::sleep(std::time::Duration::from_millis(delay));
                    delay = delay * 2; // Exponential backoff
                }
            }
        }

        Err(ProcessMiningError::ComputationError(
            "Max retries exceeded".to_string()
        ))
    }

    /// Fallback operation
    pub fn fallback<F, G, T>(
        primary: F,
        fallback: G,
    ) -> ProcessMiningResult<T>
    where
        F: FnOnce() -> ProcessMiningResult<T>,
        G: FnOnce() -> ProcessMiningResult<T>,
    {
        match primary() {
            Ok(result) => Ok(result),
            Err(_) => fallback(),
        }
    }

    /// Circuit breaker pattern
    pub struct CircuitBreaker<T> {
        failure_count: u32,
        max_failures: u32,
        timeout_duration: std::time::Duration,
        last_failure_time: Option<std::time::Instant>,
        state: CircuitBreakerState,
    }

    #[derive(Debug, Clone, PartialEq)]
    enum CircuitBreakerState {
        Closed,
        Open,
        HalfOpen,
    }

    impl<T> CircuitBreaker<T> {
        pub fn new(max_failures: u32, timeout_duration: std::time::Duration) -> Self {
            Self {
                failure_count: 0,
                max_failures,
                timeout_duration,
                last_failure_time: None,
                state: CircuitBreakerState::Closed,
            }
        }

        pub fn execute<F>(&mut self, f: F) -> ProcessMiningResult<T>
        where
            F: FnOnce() -> ProcessMiningResult<T>,
        {
            match self.state {
                CircuitBreakerState::Closed => self.execute_closed(f),
                CircuitBreakerState::Open => self.execute_open(),
                CircuitBreakerState::HalfOpen => self.execute_half_open(f),
            }
        }

        fn execute_closed<F>(&mut self, f: F) -> ProcessMiningResult<T>
        where
            F: FnOnce() -> ProcessMiningResult<T>,
        {
            match f() {
                Ok(result) => {
                    self.failure_count = 0;
                    Ok(result)
                }
                Err(error) => {
                    self.failure_count += 1;
                    if self.failure_count >= self.max_failures {
                        self.state = CircuitBreakerState::Open;
                        self.last_failure_time = Some(std::time::Instant::now());
                    }
                    Err(error)
                }
            }
        }

        fn execute_open(&self) -> ProcessMiningResult<T> {
            if let Some(last_failure) = self.last_failure_time {
                if last_failure.elapsed() < self.timeout_duration {
                    Err(ProcessMiningError::ComputationError(
                        "Circuit breaker is open".to_string()
                    ))
                } else {
                    // Time to try again
                    self.state = CircuitBreakerState::HalfOpen;
                    Err(ProcessMiningError::ComputationError(
                        "Circuit breaker moving to half-open state".to_string()
                    ))
                }
            } else {
                // This shouldn't happen in closed state
                Err(ProcessMiningError::ComputationError(
                    "Invalid circuit breaker state".to_string()
                ))
            }
        }

        fn execute_half_open<F>(&mut self, f: F) -> ProcessMiningResult<T>
        where
            F: FnOnce() -> ProcessMiningResult<T>,
        {
            match f() {
                Ok(result) => {
                    self.failure_count = 0;
                    self.state = CircuitBreakerState::Closed;
                    Ok(result)
                }
                Err(error) => {
                    self.state = CircuitBreakerState::Open;
                    self.last_failure_time = Some(std::time::Instant::now());
                    Err(error)
                }
            }
        }

        pub fn reset(&mut self) {
            self.failure_count = 0;
            self.last_failure_time = None;
            self.state = CircuitBreakerState::Closed;
        }

        pub fn state(&self) -> CircuitBreakerState {
            self.state.clone()
        }
    }
}

/// Error metrics and logging
pub mod metrics {
    use super::*;

    /// Error tracking
    pub struct ErrorTracker {
        pub errors: Vec<ErrorRecord>,
        pub error_counts: std::collections::HashMap<String, usize>,
        pub max_errors: usize,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ErrorRecord {
        pub timestamp: chrono::DateTime<chrono::Utc>,
        pub error_type: String,
        pub message: String,
        pub stack_trace: Option<String>,
        pub context: Option<String>,
    }

    impl ErrorTracker {
        pub fn new(max_errors: usize) -> Self {
            Self {
                errors: Vec::new(),
                error_counts: std::collections::HashMap::new(),
                max_errors,
            }
        }

        pub fn log_error(&mut self, error_type: String, message: String) {
            let record = ErrorRecord {
                timestamp: chrono::Utc::now(),
                error_type,
                message,
                stack_trace: std::env::var("RUST_BACKTRACE").ok().and_then(|_| {
                    std::backtrace::Backtrace::capture().to_string().into()
                }),
                context: Some(format!("Thread: {:?}", std::thread::current().id())),
            };

            self.errors.push(record.clone());
            *self.error_counts.entry(record.error_type.clone()).or_insert(0) += 1;

            // Keep only the most recent errors
            if self.errors.len() > self.max_errors {
                self.errors.remove(0);
            }
        }

        pub fn get_error_count(&self, error_type: &str) -> usize {
            self.error_counts.get(error_type).copied().unwrap_or(0)
        }

        pub fn get_top_errors(&self, n: usize) -> Vec<(String, usize)> {
            let mut errors: Vec<_> = self.error_counts.iter().collect();
            errors.sort_by(|a, b| b.1.cmp(a.1));
            errors.into_iter().take(n).map(|(k, v)| (k.clone(), v.clone())).collect()
        }

        pub fn get_recent_errors(&self, n: usize) -> Vec<&ErrorRecord> {
            self.errors.iter().rev().take(n).collect()
        }

        pub fn clear(&mut self) {
            self.errors.clear();
            self.error_counts.clear();
        }
    }

    /// Error severity levels
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum ErrorSeverity {
        Info,
        Warning,
        Error,
        Critical,
    }

    impl ErrorSeverity {
        pub fn from_error(error: &ProcessMiningError) -> Self {
            match error {
                ProcessMiningError::InvalidEventLog(_) => ErrorSeverity::Critical,
                ProcessMiningError::CaseNotFound(_) => ErrorSeverity::Warning,
                ProcessMiningError::DuplicateCaseId(_) => ErrorSeverity::Warning,
                ProcessMiningError::InvalidModel(_) => ErrorSeverity::Critical,
                ProcessMiningError::ComputationError(_) => ErrorSeverity::Error,
                ProcessMiningError::ParseError(_) => ErrorSeverity::Error,
                ProcessMiningError::ValidationError(_) => ErrorSeverity::Error,
                ProcessMiningError::IoError(_) => ErrorSeverity::Error,
                ProcessMiningError::TimeoutError(_) => ErrorSeverity::Warning,
                ProcessMiningError::MemoryError(_) => ErrorSeverity::Critical,
                ProcessMiningError::ConfigurationError(_) => ErrorSeverity::Error,
                ProcessMiningError::AlgorithmError(_) => ErrorSeverity::Error,
                ProcessMiningError::FormatError(_) => ErrorSeverity::Error,
                ProcessMiningError::StatisticalError(_) => ErrorSeverity::Error,
                ProcessMiningError::ResourceNotFound(_) => ErrorSeverity::Warning,
                ProcessMiningError::PermissionError(_) => ErrorSeverity::Warning,
                ProcessMiningError::SystemError(_) => ErrorSeverity::Critical,
            }
        }
    }

    /// Error severity to color mapping for logging
    pub fn get_severity_color(severity: ErrorSeverity) -> colored::Color {
        match severity {
            ErrorSeverity::Info => colored::Color::Cyan,
            ErrorSeverity::Warning => colored::Color::Yellow,
            ErrorSeverity::Error => colored::Color::Red,
            ErrorSeverity::Critical => colored::Color::Magenta,
        }
    }

    /// Error recovery strategies
    pub mod strategies {
        use super::*;

        /// Default recovery strategy for common errors
        pub fn default_recovery(error: &ProcessMiningError) -> Option<String> {
            match error {
                ProcessMiningError::InvalidEventLog(msg) => Some(format!(
                    "Recovery: Check XES format and validate log structure. Error: {}", msg
                )),
                ProcessMiningError::CaseNotFound(id) => Some(format!(
                    "Recovery: Try case ID '{}' or check log for case variations", id
                )),
                ProcessMiningError::DuplicateCaseId(id) => Some(format!(
                    "Recovery: Remove duplicate case ID '{}' or merge traces", id
                )),
                ProcessMiningError::InvalidModel(msg) => Some(format!(
                    "Recovery: Validate model structure and process mining constraints. Error: {}", msg
                )),
                ProcessMiningError::ComputationError(msg) => Some(format!(
                    "Recovery: Check input data and algorithm parameters. Error: {}", msg
                )),
                ProcessMiningError::TimeoutError(_) => Some(
                    "Recovery: Increase timeout or optimize algorithm parameters".to_string()
                ),
                ProcessMiningError::MemoryError(_) => Some(
                    "Recovery: Process data in smaller chunks or increase memory allocation".to_string()
                ),
                ProcessMiningError::ConfigurationError(msg) => Some(format!(
                    "Recovery: Validate configuration parameters. Error: {}", msg
                )),
                _ => None,
            }
        }

        /// Recovery strategy for memory errors
        pub fn memory_recovery(error: &ProcessMiningError) -> Option<String> {
            if matches!(error, ProcessMiningError::MemoryError(_)) {
                Some(
                    "Recovery: Try processing data in batches, increase memory limit, or use streaming approach".to_string()
                )
            } else {
                None
            }
        }

        /// Recovery strategy for timeout errors
        pub fn timeout_recovery(error: &ProcessMiningError) -> Option<String> {
            if matches!(error, ProcessMiningError::TimeoutError(_)) {
                Some(
                    "Recovery: Increase timeout, use parallel processing, or simplify algorithm".to_string()
                )
            } else {
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let error = ProcessMiningError::InvalidEventLog("Test error".to_string());
        assert_eq!(error.to_string(), "Invalid event log: Test error");
    }

    #[test]
    fn test_error_tracker() {
        let mut tracker = ErrorTracker::new(100);
        tracker.log_error("TestError".to_string(), "Test message".to_string());
        assert_eq!(tracker.get_error_count("TestError"), 1);
        assert_eq!(tracker.get_top_errors(1).len(), 1);
    }

    #[test]
    fn test_error_severity() {
        let error = ProcessMiningError::InvalidEventLog("Test".to_string());
        let severity = ErrorSeverity::from_error(&error);
        assert_eq!(severity, ErrorSeverity::Critical);
    }

    #[test]
    fn test_recovery_strategies() {
        let error = ProcessMiningError::InvalidEventLog("Test".to_string());
        let recovery = strategies::default_recovery(&error);
        assert!(recovery.is_some());
        assert!(recovery.unwrap().contains("Recovery:"));
    }

    #[test]
    fn test_circuit_breaker() {
        let mut breaker = CircuitBreaker::new(3, std::time::Duration::from_secs(1));

        // This should work normally
        let result = breaker.execute(|| Ok("Success"));
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_operation() {
        let operation = SafeOperation::new(|| Ok(42), "test operation");
        assert!(operation.result.is_ok());
        assert_eq!(operation.result.unwrap(), 42);
    }

    #[test]
    fn test_error_context() {
        let context = ErrorContext::new(5, "test context");
        let result = context.chain(|value| Ok(value * 2));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 10);
    }
}