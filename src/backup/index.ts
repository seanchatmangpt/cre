/**
 * Backup and Disaster Recovery System - Main Export
 */

// Core Manager
export { BackupManager } from './backup-manager';

// Services
export { BackupService, BackupError } from './services/backup.service';
export { ReplicationEngine, ReplicationError } from './replication/replication.engine';
export { RecoveryService, RecoveryError } from './recovery/recovery.service';
export { MonitoringService } from './monitoring/monitoring.service';

// Types
export type {
  BackupMetadata,
  ReplicationConfig,
  ReplicaStatus,
  PITRConfig,
  RestoreJobConfig,
  RestoreJobStatus,
  RestoreError,
  RTORPOGuarantees,
  BackupSchedule,
  DRTestResult,
  ReplicationStream,
  BackupStorageBackend,
  BackupMetrics,
  BackupAuditLog,
  BackupHealthCheck,
  HealthIssue,
} from './types';

// Configuration
export {
  SLATiers,
  createCriticalReplicationConfig,
  createStandardReplicationConfig,
  createLowerReplicationConfig,
  createPITRConfig72h,
  createPITRConfig30d,
  createPITRConfig24h,
  BackupSchedulePresets,
  StorageBackendTemplates,
  AlertThresholdTemplates,
  mergeConfig,
  validateReplicationConfig,
  validatePITRConfig,
} from './config/default-config';
