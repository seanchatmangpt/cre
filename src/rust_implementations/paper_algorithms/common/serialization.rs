//! Serialization and deserialization utilities for process mining data

use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use serde::{Serialize, Deserialize};
use serde_json;
use polars::prelude::*;
use csv::Writer;
use anyhow::Result;

/// Event log serialization format
#[derive(Debug, Clone)]
pub enum LogFormat {
    XES,
    CSV,
    JSON,
    Parquet,
    Arrow,
}

/// Process mining data serialization utilities
pub struct LogSerializer {
    format: LogFormat,
    compression: bool,
}

impl LogSerializer {
    pub fn new(format: LogFormat) -> Self {
        Self {
            format,
            compression: false,
        }
    }

    pub fn with_compression(mut self, enable: bool) -> Self {
        self.compression = enable;
        self
    }

    /// Serialize event log to file
    pub fn serialize<T>(&self, data: &T, path: &Path) -> Result<()>
    where
        T: Serialize,
    {
        match self.format {
            LogFormat::XES => self.serialize_xes(data, path),
            LogFormat::CSV => self.serialize_csv(data, path),
            LogFormat::JSON => self.serialize_json(data, path),
            LogFormat::Parquet => self.serialize_parquet(data, path),
            LogFormat::Arrow => self.serialize_arrow(data, path),
        }
    }

    /// Deserialize event log from file
    pub fn deserialize<T>(&self, path: &Path) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        match self.format {
            LogFormat::XES => self.deserialize_xes(path),
            LogFormat::CSV => self.deserialize_csv(path),
            LogFormat::JSON => self.deserialize_json(path),
            LogFormat::Parquet => self.deserialize_parquet(path),
            LogFormat::Arrow => self.deserialize_arrow(path),
        }
    }

    /// Serialize to XES format
    fn serialize_xes<T>(&self, data: &T, path: &Path) -> Result<()>
    where
        T: Serialize,
    {
        // Convert to JSON first, then transform to XES
        let json_data = serde_json::to_string(data)?;
        let xes_data = self.json_to_xes(&json_data)?;

        let mut file = self.open_file(path)?;
        file.write_all(xes_data.as_bytes())?;

        Ok(())
    }

    /// Serialize to CSV format
    fn serialize_csv<T>(&self, data: &T, path: &Path) -> Result<()>
    where
        T: Serialize,
    {
        let json_data = serde_json::to_string(data)?;
        let log_data: serde_json::Value = serde_json::from_str(&json_data)?;

        if let Some(events) = log_data.get("events").and_then(|v| v.as_array()) {
            let mut writer = Writer::from_path(path)?;

            // Write header
            writer.write_record(&[
                "case_id", "activity", "timestamp", "resource",
                "lifecycle", "cost", "concept_name"
            ])?;

            // Write data
            for event in events {
                writer.write_record(&[
                    event.get("case_id").and_then(|v| v.as_str()).unwrap_or(""),
                    event.get("activity").and_then(|v| v.as_str()).unwrap_or(""),
                    event.get("timestamp").and_then(|v| v.as_str()).unwrap_or(""),
                    event.get("resource").and_then(|v| v.as_str()).unwrap_or(""),
                    event.get("lifecycle").and_then(|v| v.as_str()).unwrap_or(""),
                    event.get("cost").and_then(|v| v.as_f64()).unwrap_or(0.0).to_string(),
                    event.get("concept_name").and_then(|v| v.as_str()).unwrap_or(""),
                ])?;
            }
        }

        Ok(())
    }

    /// Serialize to JSON format
    fn serialize_json<T>(&self, data: &T, path: &Path) -> Result<()>
    where
        T: Serialize,
    {
        let json_data = serde_json::to_string_pretty(data)?;
        let mut file = self.open_file(path)?;
        file.write_all(json_data.as_bytes())?;
        Ok(())
    }

    /// Serialize to Parquet format
    fn serialize_parquet<T>(&self, data: &T, path: &Path) -> Result<()>
    where
        T: Serialize,
    {
        let json_data = serde_json::to_string(data)?;
        let log_data: serde_json::Value = serde_json::from_str(&json_data)?;

        // Create DataFrame
        let mut df = df!(
            "case_id" => Vec::<String>::new(),
            "activity" => Vec::<String>::new(),
            "timestamp" => Vec::<String>::new(),
            "resource" => Vec::<Option<String>>::new(),
            "cost" => Vec::<Option<f64>>::new(),
        )?;

        if let Some(events) = log_data.get("events").and_then(|v| v.as_array()) {
            let mut case_ids = Vec::new();
            let mut activities = Vec::new();
            let mut timestamps = Vec::new();
            let mut resources = Vec::new();
            let mut costs = Vec::new();

            for event in events {
                case_ids.push(event.get("case_id").and_then(|v| v.as_str()).unwrap_or("").to_string());
                activities.push(event.get("activity").and_then(|v| v.as_str()).unwrap_or("").to_string());
                timestamps.push(event.get("timestamp").and_then(|v| v.as_str()).unwrap_or("").to_string());
                resources.push(event.get("resource").and_then(|v| v.as_str()).map(|s| s.to_string()));
                costs.push(event.get("cost").and_then(|v| v.as_f64()));
            }

            df = df.with_column(Series::new("case_id", case_ids))?;
            df = df.with_column(Series::new("activity", activities))?;
            df = df.with_column(Series::new("timestamp", timestamps))?;
            df = df.with_column(Series::new("resource", resources))?;
            df = df.with_column(Series::new("cost", costs))?;
        }

        // Write to Parquet
        let mut file = self.open_file(path)?;
        ParquetWriter::new(&mut file).finish(&mut df)?;

        Ok(())
    }

    /// Serialize to Arrow format
    fn serialize_arrow<T>(&self, data: &T, path: &Path) -> Result<()>
    where
        T: Serialize,
    {
        let json_data = serde_json::to_string(data)?;
        let log_data: serde_json::Value = serde_json::from_str(&json_data)?;

        // Create DataFrame (similar to Parquet)
        let mut df = df!(
            "case_id" => Vec::<String>::new(),
            "activity" => Vec::<String>::new(),
            "timestamp" => Vec::<String>::new(),
        )?;

        if let Some(events) = log_data.get("events").and_then(|v| v.as_array()) {
            let mut case_ids = Vec::new();
            let mut activities = Vec::new();
            let mut timestamps = Vec::new();

            for event in events {
                case_ids.push(event.get("case_id").and_then(|v| v.as_str()).unwrap_or("").to_string());
                activities.push(event.get("activity").and_then(|v| v.as_str()).unwrap_or("").to_string());
                timestamps.push(event.get("timestamp").and_then(|v| v.as_str()).unwrap_or("").to_string());
            }

            df = df.with_column(Series::new("case_id", case_ids))?;
            df = df.with_column(Series::new("activity", activities))?;
            df = df.with_column(Series::new("timestamp", timestamps))?;
        }

        // Write to Arrow
        let file = File::create(path)?;
        let mut writer = ArrowWriter::try_new(file, df.schema(), None)?;
        writer.write(&df)?;
        writer.finish()?;

        Ok(())
    }

    /// Deserialize from XES format
    fn deserialize_xes<T>(&self, path: &Path) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let mut file = File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        let json_data = self.xes_to_json(&content)?;
        serde_json::from_str(&json_data).map_err(Into::into)
    }

    /// Deserialize from CSV format
    fn deserialize_csv<T>(&self, path: &Path) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let mut reader = csv::Reader::from_path(path)?;
        let mut records = Vec::new();

        for result in reader.deserialize() {
            let record: EventRecord = result?;
            records.push(record);
        }

        let json_data = serde_json::to_string(&records)?;
        serde_json::from_str(&json_data).map_err(Into::into)
    }

    /// Deserialize from JSON format
    fn deserialize_json<T>(&self, path: &Path) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let mut file = File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        serde_json::from_str(&content).map_err(Into::into)
    }

    /// Deserialize from Parquet format
    fn deserialize_parquet<T>(&self, path: &Path) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let file = File::open(path)?;
        let df = ParquetReader::new(file).finish()?;

        // Convert DataFrame to JSON
        let json_str = df.to_json()?;
        serde_json::from_str(&json_str).map_err(Into::into)
    }

    /// Deserialize from Arrow format
    fn deserialize_arrow<T>(&self, path: &Path) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let file = File::open(path)?;
        let reader = ArrowReader::try_new(file)?;
        let df = DataFrame::try_from(reader)?;

        let json_str = df.to_json()?;
        serde_json::from_str(&json_str).map_err(Into::into)
    }

    /// Helper function to open file
    fn open_file(&self, path: &Path) -> Result<File> {
        if self.compression {
            // In production, handle compression
            OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open(path)
        } else {
            OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)
        }
    }

    /// Convert JSON to XES (simplified)
    fn json_to_xes(&self, json_data: &str) -> Result<String> {
        let mut xes = String::from(
            r#"<?xml version="1.0" encoding="UTF-8"?>
<log>
  <global>
    <string key="concept:name" value="Process Mining Log"/>
    <string key="concept:version" value="1.0"/>
  </global>
  <trace>"#
        );

        let log_data: serde_json::Value = serde_json::from_str(json_data)?;

        if let Some(events) = log_data.get("events").and_then(|v| v.as_array()) {
            for (i, event) in events.iter().enumerate() {
                if i > 0 {
                    xes.push_str("    <trace>\n");
                }

                xes.push_str(&format!(
                    r#"      <event>
        <string key="concept:name">{}</string>
        <string key="concept:caseId">{}</string>
        <string key="concept:timestamp">{}</string>
        <string key="concept:resource">{}</string>
      </event>"#,
                    event.get("activity").and_then(|v| v.as_str()).unwrap_or(""),
                    event.get("case_id").and_then(|v| v.as_str()).unwrap_or(""),
                    event.get("timestamp").and_then(|v| v.as_str()).unwrap_or(""),
                    event.get("resource").and_then(|v| v.as_str()).unwrap_or(""),
                ));
            }
        }

        xes.push_str(
            r#"
  </trace>
</log>"#
        );

        Ok(xes)
    }

    /// Convert XES to JSON (simplified)
    fn xes_to_json(&self, xes_data: &str) -> Result<String> {
        // This is a simplified implementation
        // In production, use proper XML parsing
        let mut events = Vec::new();

        // Simple parsing logic (replace with proper XML parser)
        if xes_data.contains("<event>") {
            let event_blocks: Vec<&str> = xes_data.split("<event>").collect();
            for block in event_blocks.iter().skip(1) {
                if block.contains("</event>") {
                    let event_content = block.split("</event>").next().unwrap();

                    let activity = if event_content.contains("<string key=\"concept:name\">") {
                        event_content.split("<string key=\"concept:name\">")
                            .nth(1)
                            .and_then(|s| s.split("</string>").next())
                            .unwrap_or("")
                            .to_string()
                    } else {
                        "".to_string()
                    };

                    let case_id = if event_content.contains("<string key=\"concept:caseId\">") {
                        event_content.split("<string key=\"concept:caseId\">")
                            .nth(1)
                            .and_then(|s| s.split("</string>").next())
                            .unwrap_or("")
                            .to_string()
                    } else {
                        "".to_string()
                    };

                    events.push(json!({
                        "activity": activity,
                        "case_id": case_id,
                        "timestamp": "",
                        "resource": "",
                        "lifecycle": "",
                        "cost": 0.0,
                        "concept_name": activity
                    }));
                }
            }
        }

        let log = json!({
            "events": events,
            "num_cases": events.len(),
            "num_events": events.len(),
            "activities": events.iter().map(|e| e["activity"].as_str().unwrap_or("")).collect::<Vec<_>>(),
        });

        Ok(serde_json::to_string_pretty(&log)?)
    }
}

/// Event record structure for CSV
#[derive(Debug, Deserialize, Serialize)]
pub struct EventRecord {
    pub case_id: String,
    pub activity: String,
    pub timestamp: String,
    pub resource: Option<String>,
    pub lifecycle: Option<String>,
    pub cost: Option<f64>,
    pub concept_name: Option<String>,
}

/// Batch serialization utilities
pub struct BatchSerializer {
    serializer: LogSerializer,
    batch_size: usize,
}

impl BatchSerializer {
    pub fn new(format: LogFormat, batch_size: usize) -> Self {
        Self {
            serializer: LogSerializer::new(format),
            batch_size,
        }
    }

    /// Serialize large dataset in batches
    pub fn serialize_in_batches<T, I>(&self, data: I, output_dir: &Path) -> Result<()>
    where
        T: Serialize,
        I: Iterator<Item = T>,
    {
        let mut batch = Vec::new();
        let mut batch_num = 0;

        for item in data {
            batch.push(item);

            if batch.len() >= self.batch_size {
                let batch_path = output_dir.join(format!("batch_{:04}.json", batch_num));
                self.serializer.serialize(&batch, &batch_path)?;

                batch.clear();
                batch_num += 1;
            }
        }

        // Serialize remaining items
        if !batch.is_empty() {
            let batch_path = output_dir.join(format!("batch_{:04}.json", batch_num));
            self.serializer.serialize(&batch, &batch_path)?;
        }

        Ok(())
    }

    /// Deserialize from batch files
    pub fn deserialize_from_batches<T>(&self, input_dir: &Path) -> Result<Vec<T>>
    where
        T: for<'de> Deserialize<'de>,
    {
        let mut all_data = Vec::new();

        for entry in std::fs::read_dir(input_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                let batch: Vec<T> = self.serializer.deserialize(&path)?;
                all_data.extend(batch);
            }
        }

        Ok(all_data)
    }
}

/// Stream processing utilities
pub struct StreamProcessor {
    input_path: PathBuf,
    output_path: PathBuf,
    batch_size: usize,
}

impl StreamProcessor {
    pub fn new(input: PathBuf, output: PathBuf, batch_size: usize) -> Self {
        Self {
            input_path: input,
            output_path: output,
            batch_size,
        }
    }

    /// Process event log in streaming fashion
    pub fn process<T, F, R>(&self, mut processor: F) -> Result<()>
    where
        F: Fn(T) -> Result<R>,
        T: for<'de> Deserialize<'de>,
        R: Serialize,
    {
        let input_file = File::open(&self.input_path)?;
        let reader = BufReader::new(input_file);

        let output_file = File::create(&self.output_path)?;
        let writer = BufWriter::new(output_file);

        // Process in batches
        let mut batch = Vec::new();
        let mut writer = csv::Writer::new(writer);

        for line in reader.lines() {
            let line = line?;
            let item: T = serde_json::from_str(&line)?;

            match processor(item) {
                Ok(result) => {
                    batch.push(result);

                    if batch.len() >= self.batch_size {
                        for item in &batch {
                            writer.serialize(item)?;
                        }
                        batch.clear();
                    }
                }
                Err(e) => {
                    eprintln!("Error processing item: {}", e);
                }
            }
        }

        // Write remaining items
        for item in batch {
            writer.serialize(item)?;
        }

        writer.flush()?;
        Ok(())
    }
}

/// Data validation utilities
pub struct DataValidator {
    pub rules: Vec<ValidationRule>,
}

#[derive(Debug, Clone)]
pub enum ValidationRule {
    Required(String),
    Type(String, String), // field_name, expected_type
    Range(String, f64, f64), // field_name, min, max
    Regex(String, String), // field_name, pattern
    Unique(String), // field_name
}

impl DataValidator {
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
        }
    }

    pub fn add_rule(mut self, rule: ValidationRule) -> Self {
        self.rules.push(rule);
        self
    }

    pub fn validate<T>(&self, data: &T) -> ValidationResult
    where
        T: Serialize,
    {
        let json_data = serde_json::to_value(data).unwrap_or(serde_json::Value::Null);
        let mut errors = Vec::new();

        for rule in &self.rules {
            match rule {
                ValidationRule::Required(field) => {
                    if !json_data.get(field).is_some_and(|v| !v.is_null()) {
                        errors.push(format!("Field '{}' is required", field));
                    }
                }
                ValidationRule::Type(field, expected_type) => {
                    if let Some(value) = json_data.get(field) {
                        let actual_type = match value {
                            serde_json::Value::Null => "null",
                            serde_json::Value::Bool(_) => "boolean",
                            serde_json::Value::Number(_) => "number",
                            serde_json::Value::String(_) => "string",
                            serde_json::Value::Array(_) => "array",
                            serde_json::Value::Object(_) => "object",
                        };

                        if actual_type != expected_type {
                            errors.push(format!("Field '{}' should be of type {}, but got {}", field, expected_type, actual_type));
                        }
                    }
                }
                ValidationRule::Range(field, min, max) => {
                    if let Some(serde_json::Value::Number(n)) = json_data.get(field) {
                        if let Some(value) = n.as_f64() {
                            if value < *min || value > *max {
                                errors.push(format!("Field '{}' should be between {} and {}", field, min, max));
                            }
                        }
                    }
                }
                ValidationRule::Regex(field, pattern) => {
                    if let Some(serde_json::Value::String(s)) = json_data.get(field) {
                        let re = regex::Regex::new(pattern).unwrap();
                        if !re.is_match(s) {
                            errors.push(format!("Field '{}' doesn't match pattern: {}", field, pattern));
                        }
                    }
                }
                ValidationRule::Unique(field) => {
                    // This would require more complex logic for actual uniqueness checking
                    // Simplified implementation
                    if let Some(serde_json::Value::Array(items)) = json_data.get(field) {
                        let seen_values: std::collections::HashSet<String> = items
                            .iter()
                            .filter_map(|v| v.as_str())
                            .map(|s| s.to_string())
                            .collect();

                        if seen_values.len() != items.len() {
                            errors.push(format!("Field '{}' contains duplicate values", field));
                        }
                    }
                }
            }
        }

        ValidationResult {
            is_valid: errors.is_empty(),
            errors,
        }
    }
}

/// Validation result
#[derive(Debug)]
pub struct ValidationResult {
    pub is_valid: bool,
    pub errors: Vec<String>,
}

impl ValidationResult {
    pub fn new() -> Self {
        Self {
            is_valid: true,
            errors: Vec::new(),
        }
    }

    pub fn with_errors(errors: Vec<String>) -> Self {
        Self {
            is_valid: errors.is_empty(),
            errors,
        }
    }

    pub fn is_valid(&self) -> bool {
        self.is_valid
    }

    pub fn get_errors(&self) -> &[String] {
        &self.errors
    }

    pub fn format_errors(&self) -> String {
        if self.errors.is_empty() {
            "Validation passed successfully".to_string()
        } else {
            format!("Validation failed:\n  {}", self.errors.join("\n  "))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_serializer() {
        let data = json!({
            "events": [
                {
                    "case_id": "1",
                    "activity": "A",
                    "timestamp": "2023-01-01T00:00:00Z",
                    "resource": "user1",
                    "lifecycle": "complete",
                    "cost": 100.0,
                    "concept_name": "A"
                }
            ]
        });

        let temp_dir = tempfile::tempdir().unwrap();
        let test_path = temp_dir.path().join("test.json");

        // Test JSON serialization
        let serializer = LogSerializer::new(LogFormat::JSON);
        serializer.serialize(&data, &test_path).unwrap();

        // Test JSON deserialization
        let deserialized: serde_json::Value = serializer.deserialize(&test_path).unwrap();
        assert_eq!(deserialized["events"][0]["activity"], "A");

        // Test CSV serialization
        let csv_path = temp_dir.path().join("test.csv");
        let csv_serializer = LogSerializer::new(LogFormat::CSV);
        csv_serializer.serialize(&data, &csv_path).unwrap();

        // Test CSV deserialization
        let csv_deserialized: Vec<EventRecord> = csv_serializer.deserialize(&csv_path).unwrap();
        assert_eq!(csv_deserialized[0].activity, "A");
    }

    #[test]
    fn test_batch_serializer() {
        let data: Vec<serde_json::Value> = (0..100)
            .map(|i| json!({
                "case_id": i.to_string(),
                "activity": format!("Activity_{}", i),
                "timestamp": "2023-01-01T00:00:00Z"
            }))
            .collect();

        let temp_dir = tempfile::tempdir().unwrap();
        let batch_serializer = BatchSerializer::new(LogFormat::JSON, 10);

        // Test serialization in batches
        let iter = data.into_iter();
        batch_serializer.serialize_in_batches(iter, temp_dir.path()).unwrap();

        // Test deserialization from batches
        let deserialized: Vec<serde_json::Value> = batch_serializer.deserialize_from_batches(temp_dir.path()).unwrap();
        assert_eq!(deserialized.len(), 100);
    }

    #[test]
    fn test_data_validator() {
        let data = json!({
            "case_id": "123",
            "activity": "Test",
            "timestamp": "2023-01-01T00:00:00Z",
            "cost": 50.0
        });

        let validator = DataValidator::new()
            .add_rule(ValidationRule::Required("case_id".to_string()))
            .add_rule(ValidationRule::Type("cost".to_string(), "number".to_string()))
            .add_rule(ValidationRule::Range("cost".to_string(), 0.0, 100.0));

        let result = validator.validate(&data);
        assert!(result.is_valid());
    }

    #[test]
    fn test_validation_failure() {
        let data = json!({
            "case_id": "123",
            "activity": "Test",
            "timestamp": "2023-01-01T00:00:00Z",
            "cost": -10.0  // This should fail the range validation
        });

        let validator = DataValidator::new()
            .add_rule(ValidationRule::Required("case_id".to_string()))
            .add_rule(ValidationRule::Range("cost".to_string(), 0.0, 100.0));

        let result = validator.validate(&data);
        assert!(!result.is_valid());
        assert!(result.errors.len() > 0);
    }
}