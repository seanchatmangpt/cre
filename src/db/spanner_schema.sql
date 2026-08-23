-- ===================================================================
-- Cloud Spanner Schema for CRE Workflow Engine
-- ===================================================================
--
-- This SQL DDL defines the database schema for CRE (Common Runtime
-- Environment) workflow state persistence using Google Cloud Spanner.
--
-- Tables:
--   - workflow_cases: Stores workflow execution instances
--   - work_items: Stores individual work items within cases
--   - event_log: Stores event history for cases
--   - checkpoints: Stores workflow checkpoint/recovery data
--
-- Usage:
--   gcloud spanner databases ddl update <DATABASE_ID> \
--     --instance=<INSTANCE_ID> \
--     --ddl="$(cat spanner_schema.sql)"
--
-- ===================================================================

-- ===================================================================
-- Table: workflow_cases
-- ===================================================================
-- Stores the state of workflow execution instances (cases).
-- Each case represents a single execution of a workflow specification.

CREATE TABLE workflow_cases (
    -- Primary Key: Unique case identifier
    case_id STRING(64) NOT NULL,

    -- Workflow specification identifier
    workflow_id STRING(256) NOT NULL,

    -- Workflow specification (serialized as JSON/Proto)
    spec BYTES(MAX) NOT NULL,

    -- Current execution status
    status STRING(20) NOT NULL,

    -- Case data payload (JSON)
    data JSON,

    -- Timestamps
    created_at INT64 NOT NULL,
    started_at INT64,
    completed_at INT64,
    updated_at INT64 NOT NULL,

    -- Optimistic concurrency control
    version INT64 NOT NULL DEFAULT (0),

) PRIMARY KEY (case_id);

-- ===================================================================
-- Table: work_items
-- ===================================================================
-- Stores individual work items (tasks) within workflow cases.
-- Linked to parent case via foreign key relationship.

CREATE TABLE work_items (
    -- Primary Key: Unique work item identifier
    workitem_id STRING(64) NOT NULL,

    -- Foreign Key: Parent case reference
    case_id STRING(64) NOT NULL,

    -- Task identifier from workflow specification
    task_id STRING(256) NOT NULL,

    -- Current work item status
    status STRING(20) NOT NULL,

    -- Work item data payload (JSON)
    data JSON,

    -- Timestamps
    enabled_at INT64,
    started_at INT64,
    completed_at INT64,

    -- Foreign Key to workflow_cases
    FOREIGN KEY (case_id) REFERENCES workflow_cases (case_id)
        ON DELETE CASCADE
) PRIMARY KEY (workitem_id, case_id),
    INTERLEAVE IN PARENT workflow_cases ON DELETE CASCADE;

-- ===================================================================
-- Table: event_log
-- ===================================================================
-- Stores event history for workflow cases.
-- Used for audit trails and process mining (XES export).

CREATE TABLE event_log (
    -- Primary Key: Composite of case_id and event_id
    case_id STRING(64) NOT NULL,
    event_id STRING(64) NOT NULL,

    -- Event type and data
    event_type STRING(100) NOT NULL,
    event_data JSON,

    -- Timestamp
    timestamp INT64 NOT NULL,

    -- Foreign Key to workflow_cases
    FOREIGN KEY (case_id) REFERENCES workflow_cases (case_id)
        ON DELETE CASCADE
) PRIMARY KEY (case_id, event_id),
    INTERLEAVE IN PARENT workflow_cases ON DELETE CASCADE;

-- ===================================================================
-- Table: checkpoints
-- ===================================================================
-- Stores workflow checkpoint/recovery data.
-- Enables resumption of workflows after system restart.

CREATE TABLE checkpoints (
    -- Primary Key: Composite of case_id and checkpoint_id
    case_id STRING(64) NOT NULL,
    checkpoint_id STRING(64) NOT NULL,

    -- Checkpoint data (serialized state)
    checkpoint_data BYTES(MAX) NOT NULL,

    -- Timestamp
    created_at INT64 NOT NULL,

    -- Foreign Key to workflow_cases
    FOREIGN KEY (case_id) REFERENCES workflow_cases (case_id)
        ON DELETE CASCADE
) PRIMARY KEY (case_id, checkpoint_id),
    INTERLEAVE IN PARENT workflow_cases ON DELETE CASCADE;

-- ===================================================================
-- Indexes for Performance
-- ===================================================================

-- Index: Active workflow cases by status
-- Useful for listing running/suspended workflows
CREATE INDEX idx_workflow_cases_status
    ON workflow_cases (status, created_at DESC);

-- Index: Work items by case and status
-- Useful for finding pending/active work items
CREATE INDEX idx_work_items_case_status
    ON work_items (case_id, status, enabled_at);

-- Index: Work items by task
-- Useful for tracking specific tasks across all cases
CREATE INDEX idx_work_items_task
    ON work_items (task_id, status)
    STORING (case_id, data);

-- Index: Event log by timestamp
-- Useful for time-based event queries and XES export
CREATE INDEX idx_event_log_timestamp
    ON event_log (timestamp DESC)
    STORING (event_type, event_data);

-- Index: Event log by event type
-- Useful for filtering events by type
CREATE INDEX idx_event_log_type
    ON event_log (event_type, timestamp);

-- Index: Checkpoints by creation time
-- Useful for cleanup of old checkpoints
CREATE INDEX idx_checkpoints_created
    ON checkpoints (created_at DESC);

-- ===================================================================
-- Validation and Constraints
-- ===================================================================

-- Note: Spanner doesn't support CHECK constraints directly.
-- Application-level validation should enforce:

-- workflow_cases.status values:
--   - running
--   - suspended
--   - completed
--   - cancelled
--   - failed

-- work_items.status values:
--   - enabled
--   - started
--   - completed
--   - failed
--   - cancelled

-- ===================================================================
-- Change Streams (for real-time subscriptions)
-- ===================================================================

-- Enable change stream for workflow_cases
CREATE CHANGE STREAM workflow_cases_stream
    FOR workflow_cases
    OPTIONS (retention_period = '7d');

-- Enable change stream for work_items
CREATE CHANGE STREAM work_items_stream
    FOR work_items
    OPTIONS (retention_period = '7d');

-- Enable change stream for event_log
CREATE CHANGE STREAM event_log_stream
    FOR event_log
    OPTIONS (retention_period = '30d');

-- ===================================================================
-- Partitioning for Large Scale Deployments
-- ===================================================================

-- For deployments with > 1TB of data, consider adding these
-- partitioning DDL statements (uncomment as needed):

-- ALTER TABLE workflow_cases
--     PARTITION BY HASH (case_id) PARTITIONS 16;

-- ===================================================================
-- Sample Queries for Common Operations
-- ===================================================================

-- 1. Get all active workflow cases:
--    SELECT * FROM workflow_cases
--    WHERE status IN ('running', 'suspended')
--    ORDER BY created_at DESC;

-- 2. Get work items for a case:
--    SELECT * FROM work_items
--    WHERE case_id = @case_id
--    ORDER BY enabled_at ASC;

-- 3. Get event history for a case:
--    SELECT * FROM event_log
--    WHERE case_id = @case_id
--    ORDER BY timestamp ASC;

-- 4. Get pending work items across all cases:
--    SELECT * FROM work_items
--    WHERE status = 'enabled'
--    ORDER BY enabled_at ASC
--    LIMIT 100;

-- 5. Get cases by workflow specification:
--    SELECT * FROM workflow_cases
--    WHERE workflow_id = @workflow_id
--    ORDER BY created_at DESC;

-- 6. Get recent events for XES export:
--    SELECT e.*, w.workflow_id
--    FROM event_log e
--    JOIN workflow_cases w ON e.case_id = w.case_id
--    WHERE e.timestamp >= @start_time
--    ORDER BY e.timestamp ASC;
