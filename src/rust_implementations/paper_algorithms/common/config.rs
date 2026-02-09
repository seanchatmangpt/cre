//! Configuration management for process mining algorithms

use std::collections::HashMap;
use std::path::PathBuf;
use serde::{Serialize, Deserialize};
use anyhow::Result;

/// Main configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessMiningConfig {
    pub general: GeneralConfig,
    pub algorithms: AlgorithmConfig,
    pub logging: LoggingConfig,
    pub performance: PerformanceConfig,
    pub memory: MemoryConfig,
    pub network: NetworkConfig,
    pub storage: StorageConfig,
    pub security: SecurityConfig,
}

/// General configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneralConfig {
    pub algorithm_name: String,
    pub version: String,
    pub output_format: String,
    pub case_id_column: String,
    pub activity_column: String,
    pub timestamp_column: String,
    pub resource_column: Option<String>,
    pub enable_validation: bool,
    pub max_concurrent_tasks: usize,
    pub default_timeout: u64,
}

/// Algorithm-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlgorithmConfig {
    pub alpha_threshold: f64,
    pub heuristic_threshold: f64,
    pub conformance_threshold: f64,
    pub fitness_threshold: f64,
    pub precision_threshold: f64,
    pub generalization_threshold: f64,
    pub local_process_threshold: f64,
    pub uncertain_event_confidence: f64,
    pub llm_model_name: String,
    pub llm_temperature: f64,
    pub petri_net_max_size: usize,
    pub max_iteration_count: usize,
    pub convergence_threshold: f64,
    pub significance_level: f64,
    pub confidence_level: f64,
}

/// Logging configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoggingConfig {
    pub enable_logging: bool,
    pub log_level: String,
    pub log_file: PathBuf,
    pub log_format: String,
    pub enable_json_format: bool,
    pub max_log_files: usize,
    pub max_log_file_size_mb: u64,
    pub enable_performance_metrics: bool,
    pub enable_memory_tracking: bool,
    pub enable_context_tracking: bool,
    pub enable_error_details: bool,
}

/// Performance configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceConfig {
    pub enable_benchmarking: bool,
    pub benchmark_iterations: usize,
    pub warmup_iterations: usize,
    pub enable_profiling: bool,
    pub profiling_output: PathBuf,
    pub enable_cache: bool,
    pub cache_size_mb: usize,
    pub enable_parallel_processing: bool,
    pub parallel_workers: usize,
    pub enable_streaming: bool,
    pub batch_size: usize,
    pub enable_real_time_monitoring: bool,
    pub monitoring_interval_ms: u64,
}

/// Memory configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryConfig {
    pub max_memory_mb: u64,
    pub enable_memory_optimization: bool,
    pub enable_garbage_collection: bool,
    pub gc_interval_ms: u64,
    pub enable_memory_profiling: bool,
    pub enable_memory_limits: bool,
    pub enable_memory_tracing: bool,
    pub enable_memory_auditing: bool,
    pub enable_memory_tracking: bool,
}

/// Network configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConfig {
    pub enable_network_features: bool,
    pub network_timeout_ms: u64,
    pub enable_ssl: bool,
    pub ssl_verify_certificate: bool,
    pub max_concurrent_requests: usize,
    pub request_retry_count: usize,
    pub request_retry_delay_ms: u64,
    pub enable_request_caching: bool,
    pub cache_ttl_seconds: u64,
    pub enable_rate_limiting: bool,
    pub rate_limit_requests_per_second: f64,
}

/// Storage configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageConfig {
    pub data_directory: PathBuf,
    pub temp_directory: PathBuf,
    pub enable_compression: bool,
    pub compression_level: i32,
    pub enable_encryption: bool,
    pub encryption_algorithm: String,
    pub max_file_size_mb: u64,
    pub max_concurrent_writes: usize,
    pub enable_write_caching: bool,
    pub cache_flush_interval_ms: u64,
    pub enable_disk_monitoring: bool,
    pub disk_usage_threshold_percent: f64,
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityConfig {
    pub enable_authentication: bool,
    pub enable_authorization: bool,
    pub encryption_key: Option<String>,
    pub enable_audit_logging: bool,
    pub audit_log_file: PathBuf,
    pub enable_data_masking: bool,
    pub sensitive_fields: Vec<String>,
    pub enable_input_validation: bool,
    pub enable_output_sanitization: bool,
    pub enable_session_management: bool,
    pub session_timeout_ms: u64,
    pub enable_rate_limiting: bool,
    pub max_requests_per_minute: u64,
}

impl Default for ProcessMiningConfig {
    fn default() -> Self {
        Self {
            general: GeneralConfig::default(),
            algorithms: AlgorithmConfig::default(),
            logging: LoggingConfig::default(),
            performance: PerformanceConfig::default(),
            memory: MemoryConfig::default(),
            network: NetworkConfig::default(),
            storage: StorageConfig::default(),
            security: SecurityConfig::default(),
        }
    }
}

impl Default for GeneralConfig {
    fn default() -> Self {
        Self {
            algorithm_name: "alpha_algorithm".to_string(),
            version: "1.0.0".to_string(),
            output_format: "json".to_string(),
            case_id_column: "case_id".to_string(),
            activity_column: "activity".to_string(),
            timestamp_column: "timestamp".to_string(),
            resource_column: Some("resource".to_string()),
            enable_validation: true,
            max_concurrent_tasks: 4,
            default_timeout: 300_000, // 5 minutes
        }
    }
}

impl Default for AlgorithmConfig {
    fn default() -> Self {
        Self {
            alpha_threshold: 0.05,
            heuristic_threshold: 0.8,
            conformance_threshold: 0.9,
            fitness_threshold: 0.8,
            precision_threshold: 0.7,
            generalization_threshold: 0.6,
            local_process_threshold: 0.5,
            uncertain_event_confidence: 0.75,
            llm_model_name: "gpt-4".to_string(),
            llm_temperature: 0.7,
            petri_net_max_size: 1000,
            max_iteration_count: 1000,
            convergence_threshold: 1e-6,
            significance_level: 0.05,
            confidence_level: 0.95,
        }
    }
}

impl Default for LoggingConfig {
    fn default() -> Self {
        Self {
            enable_logging: true,
            log_level: "info".to_string(),
            log_file: PathBuf::from("logs/process_mining.log"),
            log_format: "text".to_string(),
            enable_json_format: false,
            max_log_files: 5,
            max_log_file_size_mb: 10,
            enable_performance_metrics: true,
            enable_memory_tracking: true,
            enable_context_tracking: true,
            enable_error_details: true,
        }
    }
}

impl Default for PerformanceConfig {
    fn default() -> Self {
        Self {
            enable_benchmarking: true,
            benchmark_iterations: 100,
            warmup_iterations: 10,
            enable_profiling: false,
            profiling_output: PathBuf::from("profiling_results.json"),
            enable_cache: true,
            cache_size_mb: 512,
            enable_parallel_processing: true,
            parallel_workers: 0, // Use all available CPUs
            enable_streaming: false,
            batch_size: 1000,
            enable_real_time_monitoring: false,
            monitoring_interval_ms: 1000,
        }
    }
}

impl Default for MemoryConfig {
    fn default() -> Self {
        Self {
            max_memory_mb: 4096,
            enable_memory_optimization: true,
            enable_garbage_collection: true,
            gc_interval_ms: 5000,
            enable_memory_profiling: false,
            enable_memory_limits: true,
            enable_memory_tracing: false,
            enable_memory_auditing: false,
            enable_memory_tracking: true,
        }
    }
}

impl Default for NetworkConfig {
    fn default() -> Self {
        Self {
            enable_network_features: false,
            network_timeout_ms: 30_000,
            enable_ssl: true,
            ssl_verify_certificate: true,
            max_concurrent_requests: 10,
            request_retry_count: 3,
            request_retry_delay_ms: 1000,
            enable_request_caching: false,
            cache_ttl_seconds: 3600,
            enable_rate_limiting: true,
            rate_limit_requests_per_second: 10.0,
        }
    }
}

impl Default for StorageConfig {
    fn default() -> Self {
        Self {
            data_directory: PathBuf::from("data"),
            temp_directory: PathBuf::from("temp"),
            enable_compression: true,
            compression_level: 6,
            enable_encryption: false,
            encryption_algorithm: "AES-256".to_string(),
            max_file_size_mb: 100,
            max_concurrent_writes: 5,
            enable_write_caching: true,
            cache_flush_interval_ms: 5000,
            enable_disk_monitoring: true,
            disk_usage_threshold_percent: 80.0,
        }
    }
}

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            enable_authentication: false,
            enable_authorization: false,
            encryption_key: None,
            enable_audit_logging: false,
            audit_log_file: PathBuf::from("audit.log"),
            enable_data_masking: false,
            sensitive_fields: Vec::new(),
            enable_input_validation: true,
            enable_output_sanitization: true,
            enable_session_management: false,
            session_timeout_ms: 30_000,
            enable_rate_limiting: false,
            max_requests_per_minute: 1000,
        }
    }
}

/// Configuration builder
pub struct ConfigBuilder {
    config: ProcessMiningConfig,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        Self {
            config: ProcessMiningConfig::default(),
        }
    }

    pub fn algorithm_name(mut self, name: String) -> Self {
        self.config.general.algorithm_name = name;
        self
    }

    pub fn with_alpha_threshold(mut self, threshold: f64) -> Self {
        self.config.algorithms.alpha_threshold = threshold;
        self
    }

    pub fn with_heuristic_threshold(mut self, threshold: f64) -> Self {
        self.config.algorithms.heuristic_threshold = threshold;
        self
    }

    pub fn with_conformance_threshold(mut self, threshold: f64) -> Self {
        self.config.algorithms.conformance_threshold = threshold;
        self
    }

    pub fn with_logging(mut self, enable: bool) -> Self {
        self.config.logging.enable_logging = enable;
        self
    }

    pub fn with_benchmarking(mut self, enable: bool) -> Self {
        self.config.performance.enable_benchmarking = enable;
        self
    }

    pub fn with_parallel_workers(mut self, workers: usize) -> Self {
        self.config.performance.parallel_workers = workers;
        self
    }

    pub fn with_memory_limit(mut self, limit_mb: u64) -> Self {
        self.config.memory.max_memory_mb = limit_mb;
        self
    }

    pub fn with_output_format(mut self, format: String) -> Self {
        self.config.general.output_format = format;
        self
    }

    pub fn with_case_column(mut self, column: String) -> Self {
        self.config.general.case_id_column = column;
        self
    }

    pub fn with_activity_column(mut self, column: String) -> Self {
        self.config.general.activity_column = column;
        self
    }

    pub fn with_timestamp_column(mut self, column: String) -> Self {
        self.config.general.timestamp_column = column;
        self
    }

    pub fn build(self) -> ProcessMiningConfig {
        self.config
    }
}

/// Configuration manager
pub struct ConfigManager {
    pub config: ProcessMiningConfig,
    pub config_file: PathBuf,
}

impl ConfigManager {
    pub fn new(config_file: PathBuf) -> Result<Self> {
        let config = Self::load_config(&config_file)?;
        Ok(Self { config, config_file })
    }

    pub fn load_config(config_file: &PathBuf) -> Result<ProcessMiningConfig> {
        let content = std::fs::read_to_string(config_file)?;
        let config: ProcessMiningConfig = serde_json::from_str(&content)?;
        Ok(config)
    }

    pub fn save_config(&self) -> Result<()> {
        let content = serde_json::to_string_pretty(&self.config)?;
        std::fs::write(&self.config_file, content)?;
        Ok(())
    }

    pub fn update_config<F>(&mut self, update_func: F) -> Result<()>
    where
        F: FnOnce(&mut ProcessMiningConfig),
    {
        update_func(&mut self.config);
        self.save_config()
    }

    pub fn get_algorithm_config(&self) -> &AlgorithmConfig {
        &self.config.algorithms
    }

    pub fn get_performance_config(&self) -> &PerformanceConfig {
        &self.config.performance
    }

    pub fn get_logging_config(&self) -> &LoggingConfig {
        &self.config.logging
    }

    pub fn validate_config(&self) -> Result<()> {
        // Validate threshold values
        if self.config.algorithms.alpha_threshold < 0.0 || self.config.algorithms.alpha_threshold > 1.0 {
            return Err(anyhow::anyhow!("Alpha threshold must be between 0.0 and 1.0"));
        }

        if self.config.algorithms.heuristic_threshold < 0.0 || self.config.algorithms.heuristic_threshold > 1.0 {
            return Err(anyhow::anyhow!("Heuristic threshold must be between 0.0 and 1.0"));
        }

        if self.config.algorithms.conformance_threshold < 0.0 || self.config.algorithms.conformance_threshold > 1.0 {
            return Err(anyhow::anyhow!("Conformance threshold must be between 0.0 and 1.0"));
        }

        // Validate memory limits
        if self.config.memory.max_memory_mb < 128 {
            return Err(anyhow::anyhow!("Memory limit must be at least 128MB"));
        }

        // Validate performance settings
        if self.config.performance.benchmark_iterations < 1 {
            return Err(anyhow::anyhow!("Benchmark iterations must be at least 1"));
        }

        // Validate network settings
        if self.config.network.rate_limit_requests_per_second < 0.0 {
            return Err(anyhow::anyhow!("Rate limit must be non-negative"));
        }

        // Validate storage settings
        if self.config.storage.disk_usage_threshold_percent < 0.0 || self.config.storage.disk_usage_threshold_percent > 100.0 {
            return Err(anyhow::anyhow!("Disk usage threshold must be between 0.0 and 100.0"));
        }

        Ok(())
    }

    pub fn get_config_summary(&self) -> String {
        format!(
            "Configuration Summary\n\
            ===================\n\
            Algorithm: {}\n\
            Version: {}\n\
            Output Format: {}\n\
            Max Concurrent Tasks: {}\n\
            Timeout (ms): {}\n\
            Enable Logging: {}\n\
            Enable Benchmarking: {}\n\
            Parallel Workers: {}\n\
            Max Memory (MB): {}\n\
            Alpha Threshold: {:.3}\n\
            Heuristic Threshold: {:.3}\n\
            Conformance Threshold: {:.3}\n\
            Fitness Threshold: {:.3}\n\
            Precision Threshold: {:.3}\n\
            Generalization Threshold: {:.3}",
            self.config.general.algorithm_name,
            self.config.general.version,
            self.config.general.output_format,
            self.config.general.max_concurrent_tasks,
            self.config.general.default_timeout,
            self.config.logging.enable_logging,
            self.config.performance.enable_benchmarking,
            self.config.performance.parallel_workers,
            self.config.memory.max_memory_mb,
            self.config.algorithms.alpha_threshold,
            self.config.algorithms.heuristic_threshold,
            self.config.algorithms.conformance_threshold,
            self.config.algorithms.fitness_threshold,
            self.config.algorithms.precision_threshold,
            self.config.algorithms.generalization_threshold
        )
    }
}

/// Environment-based configuration
pub struct EnvironmentConfig {
    pub overrides: HashMap<String, String>,
}

impl EnvironmentConfig {
    pub fn new() -> Self {
        Self {
            overrides: Self::load_environment_variables(),
        }
    }

    fn load_environment_variables() -> HashMap<String, String> {
        let mut overrides = HashMap::new();

        // Common environment variables
        if let Ok(value) = std::env::var("PROCESS_MINING_ALPHA_THRESHOLD") {
            overrides.insert("algorithms.alpha_threshold".to_string(), value);
        }

        if let Ok(value) = std::env::var("PROCESS_MINING_HEURISTIC_THRESHOLD") {
            overrides.insert("algorithms.heuristic_threshold".to_string(), value);
        }

        if let Ok(value) = std::env::var("PROCESS_MINING_MAX_MEMORY_MB") {
            overrides.insert("memory.max_memory_mb".to_string(), value);
        }

        if let Ok(value) = std::env::var("PROCESS_MINING_LOG_LEVEL") {
            overrides.insert("logging.log_level".to_string(), value);
        }

        if let Ok(value) = std::env::var("PROCESS_MINING_PARALLEL_WORKERS") {
            overrides.insert("performance.parallel_workers".to_string(), value);
        }

        overrides
    }

    pub fn apply_overrides(&self, mut config: ProcessMiningConfig) -> ProcessMiningConfig {
        for (key, value) in &self.overrides {
            if key.starts_with("algorithms.") {
                let mut parts = key.split('.').collect::<Vec<_>>();
                parts.remove(0); // Remove "algorithms"
                self.apply_nested_value(&mut config.algorithms, &parts, &value);
            } else if key.starts_with("logging.") {
                let mut parts = key.split('.').collect::<Vec<_>>();
                parts.remove(0); // Remove "logging"
                self.apply_nested_value(&mut config.logging, &parts, &value);
            } else if key.starts_with("performance.") {
                let mut parts = key.split('.').collect::<Vec<_>>();
                parts.remove(0); // Remove "performance"
                self.apply_nested_value(&mut config.performance, &parts, &value);
            } else if key.starts_with("memory.") {
                let mut parts = key.split('.').collect::<Vec<_>>();
                parts.remove(0); // Remove "memory"
                self.apply_nested_value(&mut config.memory, &parts, &value);
            }
        }

        config
    }

    fn apply_nested_value<T>(&self, target: &mut T, parts: &[&str], value: &str)
    where
        T: serde::Serialize + for<'de> serde::Deserialize<'de>,
    {
        // This is a simplified implementation
        // In production, use proper nested field access or a macro
        if parts.len() == 1 {
            match parts[0] {
                _ => {
                    // Simplified - in production, use proper field access
                    eprintln!("Warning: Unsupported configuration override: {}", key);
                }
            }
        }
    }
}

/// Default configuration presets
pub struct ConfigPresets;

impl ConfigPresets {
    pub fn high_performance() -> ProcessMiningConfig {
        ProcessMiningConfig::default()
    }

    pub fn memory_efficient() -> ProcessMiningConfig {
        let mut config = ProcessMiningConfig::default();
        config.memory.max_memory_mb = 1024;
        config.memory.enable_memory_optimization = true;
        config.memory.enable_garbage_collection = true;
        config.memory.gc_interval_ms = 1000;
        config.performance.enable_cache = false;
        config.performance.parallel_workers = 1;
        config
    }

    pub fn batch_processing() -> ProcessMiningConfig {
        let mut config = ProcessMiningConfig::default();
        config.performance.enable_streaming = true;
        config.performance.batch_size = 10000;
        config.performance.enable_parallel_processing = true;
        config.storage.enable_write_caching = true;
        config.storage.cache_flush_interval_ms = 1000;
        config
    }

    pub fn real_time_processing() -> ProcessMiningConfig {
        let mut config = ProcessMiningConfig::default();
        config.performance.enable_real_time_monitoring = true;
        config.performance.monitoring_interval_ms = 100;
        config.performance.enable_parallel_processing = true;
        config.general.enable_validation = false; // Skip validation for speed
        config
    }

    pub fn development() -> ProcessMiningConfig {
        let mut config = ProcessMiningConfig::default();
        config.logging.log_level = "debug".to_string();
        config.logging.enable_logging = true;
        config.logging.enable_performance_metrics = true;
        config.logging.enable_memory_tracking = true;
        config.performance.enable_profiling = true;
        config.performance.enable_benchmarking = true;
        config.memory.enable_memory_profiling = true;
        config
    }

    pub fn production() -> ProcessMiningConfig {
        let mut config = ProcessMiningConfig::default();
        config.logging.log_level = "info".to_string();
        config.logging.enable_logging = true;
        config.logging.enable_performance_metrics = false;
        config.logging.enable_memory_tracking = false;
        config.performance.enable_profiling = false;
        config.performance.enable_benchmarking = false;
        config.memory.enable_memory_profiling = false;
        config.security.enable_authentication = true;
        config.security.enable_authorization = true;
        config.security.enable_audit_logging = true;
        config.security.enable_input_validation = true;
        config.security.enable_output_sanitization = true;
        config
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_builder() {
        let config = ConfigBuilder::new()
            .algorithm_name("test_algorithm".to_string())
            .with_alpha_threshold(0.1)
            .with_heuristic_threshold(0.9)
            .with_parallel_workers(8)
            .with_memory_limit(2048)
            .build();

        assert_eq!(config.general.algorithm_name, "test_algorithm");
        assert_eq!(config.algorithms.alpha_threshold, 0.1);
        assert_eq!(config.algorithms.heuristic_threshold, 0.9);
        assert_eq!(config.performance.parallel_workers, 8);
        assert_eq!(config.memory.max_memory_mb, 2048);
    }

    #[test]
    fn test_config_validation() {
        let mut config = ProcessMiningConfig::default();
        config.algorithms.alpha_threshold = 1.5; // Invalid

        let manager = ConfigManager {
            config,
            config_file: PathBuf::from("test_config.json"),
        };

        assert!(manager.validate_config().is_err());
    }

    #[test]
    fn test_environment_config() {
        std::env::set_var("PROCESS_MINING_ALPHA_THRESHOLD", "0.1");
        std::env::set_var("PROCESS_MINING_MAX_MEMORY_MB", "2048");

        let env_config = EnvironmentConfig::new();
        let mut config = ProcessMiningConfig::default();

        let overridden_config = env_config.apply_overrides(config.clone());

        // Note: The actual test depends on the implementation details
        // This is a placeholder test
        assert!(overridden_config.algorithms.alpha_threshold == 0.1);
    }

    #[test]
    fn test_config_presets() {
        let high_perf = ConfigPresets::high_performance();
        let mem_eff = ConfigPresets::memory_efficient();
        let batch = ConfigPresets::batch_processing();
        let realtime = ConfigPresets::real_time_processing();
        let dev = ConfigPresets::development();
        let prod = ConfigPresets::production();

        assert_eq!(high_perf.memory.max_memory_mb, 4096);
        assert_eq!(mem_eff.memory.max_memory_mb, 1024);
        assert_eq!(batch.performance.batch_size, 10000);
        assert_eq!(realtime.performance.monitoring_interval_ms, 100);
        assert_eq!(dev.logging.log_level, "debug");
        assert_eq!(prod.security.enable_authentication, true);
    }
}