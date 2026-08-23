//! Utility functions for the CRE Rust NIF
//!
//! This module provides helper functions for common operations
//! such as ID generation, memory tracking, and data validation.

use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

/// Counter for generating unique IDs
static ID_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Generate a unique ID for resources
///
/// Uses an atomic counter to ensure thread-safe ID generation.
pub fn generate_id() -> String {
    let id = ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();

    format!("{}_{}", timestamp, id)
}

/// Generate a numeric ID
///
/// Uses an atomic counter for simple numeric IDs.
pub fn generate_numeric_id() -> usize {
    ID_COUNTER.fetch_add(1, Ordering::SeqCst) as usize
}

/// Reset the ID counter (useful for testing)
#[cfg(test)]
pub fn reset_id_counter() {
    ID_COUNTER.store(1, Ordering::SeqCst);
}

/// Get current memory usage in bytes
///
/// This is a simplified implementation. For production use,
/// consider using a proper memory profiling library.
pub fn get_memory_usage() -> usize {
    // Get thread-local allocation stats if available
    #[cfg(feature = "jemalloc")]
    {
        jemalloc_ctl::stats::allocated().unwrap_or(0)
    }

    #[cfg(not(feature = "jemalloc"))]
    {
        // Return a placeholder value
        0
    }
}

/// Validate an activity name
///
/// Activity names should be non-empty and not contain special characters.
pub fn validate_activity_name(name: &str) -> Result<(), String> {
    if name.is_empty() {
        return Err("Activity name cannot be empty".to_string());
    }

    if name.len() > 256 {
        return Err("Activity name too long (max 256 characters)".to_string());
    }

    // Check for invalid characters
    if name.contains(|c: char| !c.is_alphanumeric() && c != '_' && c != '-') {
        return Err("Activity name contains invalid characters".to_string());
    }

    Ok(())
}

/// Validate a case ID
///
/// Case IDs should be non-empty strings.
pub fn validate_case_id(id: &str) -> Result<(), String> {
    if id.is_empty() {
        return Err("Case ID cannot be empty".to_string());
    }

    if id.len() > 256 {
        return Err("Case ID too long (max 256 characters)".to_string());
    }

    Ok(())
}

/// Validate a list of traces
///
/// Traces should be non-empty and contain valid activities.
pub fn validate_traces(traces: &[Vec<String>]) -> Result<(), String> {
    if traces.is_empty() {
        return Err("Trace list cannot be empty".to_string());
    }

    for (i, trace) in traces.iter().enumerate() {
        if trace.is_empty() {
            return Err(format!("Trace {} is empty", i));
        }

        for (j, activity) in trace.iter().enumerate() {
            if let Err(e) = validate_activity_name(activity) {
                return Err(format!("Trace {}, activity {}: {}", i, j, e));
            }
        }
    }

    Ok(())
}

/// Calculate fitness score between model and log
///
/// Simplified fitness calculation based on trace coverage.
pub fn calculate_fitness(
    log_traces: usize,
    model_allowed_traces: usize,
) -> f64 {
    if model_allowed_traces == 0 {
        return 0.0;
    }

    (log_traces as f64 / model_allowed_traces as f64).min(1.0)
}

/// Calculate precision score for a model
///
/// Simplified precision based on model behavior entropy.
pub fn calculate_precision(
    model_edges: usize,
    log_edges: usize,
) -> f64 {
    if model_edges == 0 {
        return 0.0;
    }

    if log_edges == 0 {
        return 1.0;
    }

    (log_edges as f64 / model_edges as f64).min(1.0)
}

/// Normalize a score to [0, 1] range
pub fn normalize_score(score: f64, min: f64, max: f64) -> f64 {
    if max == min {
        return 0.5;
    }

    ((score - min) / (max - min)).max(0.0).min(1.0)
}

/// Parse a float from an Erlang term with bounds checking
pub fn parse_float_bounded(value: f64, min: f64, max: f64, default: f64) -> f64 {
    if value.is_nan() || value.is_infinite() {
        return default;
    }

    value.clamp(min, max)
}

/// Parse a usize from an Erlang term with bounds checking
pub fn parse_usize_bounded(value: i64, min: usize, max: usize, default: usize) -> usize {
    if value < 0 {
        return default;
    }

    let as_usize = value as usize;

    if as_usize < min || as_usize > max {
        return default;
    }

    as_usize
}

/// Measure execution time of a function
pub fn measure_time<F, R>(f: F) -> (R, u64)
where
    F: FnOnce() -> R,
{
    let start = std::time::Instant::now();
    let result = f();
    let elapsed = start.elapsed().as_millis() as u64;
    (result, elapsed)
}

/// Format a duration in milliseconds to a human-readable string
pub fn format_duration_ms(ms: u64) -> String {
    if ms < 1000 {
        format!("{}ms", ms)
    } else if ms < 60_000 {
        format!("{:.1}s", ms as f64 / 1000.0)
    } else if ms < 3_600_000 {
        let minutes = ms / 60_000;
        let seconds = (ms % 60_000) / 1000;
        format!("{}m {}s", minutes, seconds)
    } else {
        let hours = ms / 3_600_000;
        let minutes = (ms % 3_600_000) / 60_000;
        format!("{}h {}m", hours, minutes)
    }
}

/// Format a memory size in bytes to a human-readable string
pub fn format_memory_bytes(bytes: usize) -> String {
    const KB: usize = 1024;
    const MB: usize = KB * 1024;
    const GB: usize = MB * 1024;

    if bytes < KB {
        format!("{}B", bytes)
    } else if bytes < MB {
        format!("{:.1}KB", bytes as f64 / KB as f64)
    } else if bytes < GB {
        format!("{:.1}MB", bytes as f64 / MB as f64)
    } else {
        format!("{:.1}GB", bytes as f64 / GB as f64)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_id() {
        reset_id_counter();
        let id1 = generate_id();
        let id2 = generate_id();

        assert_ne!(id1, id2);
        assert!(id1.contains("_"));
    }

    #[test]
    fn test_validate_activity_name() {
        assert!(validate_activity_name("valid_name").is_ok());
        assert!(validate_activity_name("valid-name").is_ok());
        assert!(validate_activity_name("").is_err());
        assert!(validate_activity_name("invalid name").is_err());
    }

    #[test]
    fn test_validate_case_id() {
        assert!(validate_case_id("case_123").is_ok());
        assert!(validate_case_id("").is_err());
    }

    #[test]
    fn test_validate_traces() {
        let valid = vec![
            vec!["a".to_string(), "b".to_string()],
            vec!["a".to_string(), "c".to_string()],
        ];
        assert!(validate_traces(&valid).is_ok());

        let empty: Vec<Vec<String>> = vec![];
        assert!(validate_traces(&empty).is_err());

        let with_empty_trace = vec![
            vec!["a".to_string()],
            vec![],
        ];
        assert!(validate_traces(&with_empty_trace).is_err());
    }

    #[test]
    fn test_calculate_fitness() {
        assert_eq!(calculate_fitness(10, 10), 1.0);
        assert_eq!(calculate_fitness(5, 10), 0.5);
        assert_eq!(calculate_fitness(15, 10), 1.0);  // Capped at 1.0
        assert_eq!(calculate_fitness(0, 10), 0.0);
        assert_eq!(calculate_fitness(10, 0), 0.0);
    }

    #[test]
    fn test_calculate_precision() {
        assert_eq!(calculate_precision(10, 10), 1.0);
        assert_eq!(calculate_precision(20, 10), 0.5);
        assert_eq!(calculate_precision(0, 10), 0.0);
        assert_eq!(calculate_precision(10, 0), 1.0);
    }

    #[test]
    fn test_normalize_score() {
        assert_eq!(normalize_score(0.5, 0.0, 1.0), 0.5);
        assert_eq!(normalize_score(0.0, 0.0, 1.0), 0.0);
        assert_eq!(normalize_score(1.0, 0.0, 1.0), 1.0);
        assert_eq!(normalize_score(2.0, 0.0, 1.0), 1.0);  // Capped
        assert_eq!(normalize_score(-1.0, 0.0, 1.0), 0.0);  // Floored
    }

    #[test]
    fn test_parse_float_bounded() {
        assert_eq!(parse_float_bounded(0.5, 0.0, 1.0, 0.5), 0.5);
        assert_eq!(parse_float_bounded(1.5, 0.0, 1.0, 0.5), 1.0);  // Capped
        assert_eq!(parse_float_bounded(-0.5, 0.0, 1.0, 0.5), 0.0); // Floored
        assert_eq!(parse_float_bounded(f64::NAN, 0.0, 1.0, 0.5), 0.5); // Default
    }

    #[test]
    fn test_parse_usize_bounded() {
        assert_eq!(parse_usize_bounded(5, 0, 10, 5), 5);
        assert_eq!(parse_usize_bounded(15, 0, 10, 5), 10); // Capped
        assert_eq!(parse_usize_bounded(-1, 0, 10, 5), 5); // Default
    }

    #[test]
    fn test_measure_time() {
        let (result, elapsed) = measure_time(|| {
            std::thread::sleep(std::time::Duration::from_millis(10));
            42
        });

        assert_eq!(result, 42);
        assert!(elapsed >= 10);
    }

    #[test]
    fn test_format_duration_ms() {
        assert_eq!(format_duration_ms(500), "500ms");
        assert_eq!(format_duration_ms(1500), "1.5s");
        assert_eq!(format_duration_ms(70000), "1m 10s");
    }

    #[test]
    fn test_format_memory_bytes() {
        assert_eq!(format_memory_bytes(500), "500B");
        assert_eq!(format_memory_bytes(2048), "2.0KB");
        assert_eq!(format_memory_bytes(2_097_152), "2.0MB");
    }
}
