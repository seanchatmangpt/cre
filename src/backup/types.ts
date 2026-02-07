/**
 * Backup and Disaster Recovery Type Definitions
 */

// Backup Metadata and Configuration
export interface BackupMetadata {
  backupId: string;
  timestamp: Date;
  type: 'FULL' | 'INCREMENTAL' | 'DIFFERENTIAL';
  sourceRegion: string;
  sourceDatabase: string;
  size: number;
  duration: number; // milliseconds
  status: 'PENDING' | 'IN_PROGRESS' | 'COMPLETED' | 'FAILED' | 'EXPIRED';
  checksum: string;
  retentionDays: number;
  expiryDate: Date;
  incrementalBaseId?: string; // For incremental backups
  tags: Record<string, string>;
}

// Replication Configuration
export interface ReplicationConfig {
  primaryRegion: string;
  secondaryRegions: string[];
  replicationMode: 'SYNCHRONOUS' | 'ASYNCHRONOUS';
  maxReplicationLagMs: number; // RPO
  targetRTOMs: number; // Recovery Time Objective
  crossRegionBandwidthMbps: number;
  compressionLevel: 'NONE' | 'LOW' | 'MEDIUM' | 'HIGH';
  encryptionEnabled: boolean;
  validationEnabled: boolean;
}

// Multi-region Replica Status
export interface ReplicaStatus {
  region: string;
  status: 'HEALTHY' | 'DEGRADED' | 'OFFLINE' | 'SYNCING';
  lastSyncTime: Date;
  lagMs: number;
  dataSize: number;
  checksumMatch: boolean;
  lastValidation: Date;
}

// Point-in-Time Recovery Configuration
export interface PITRConfig {
  enabled: boolean;
  retentionHours: number;
  snapshotIntervalMs: number;
  transactionLogRetentionDays: number;
  minRestorableTime: Date;
  maxRestorableTime: Date;
}

// Recovery Point and Objective Guarantees
export interface RTORPOGuarantees {
  rto: {
    targetMs: number;
    currentMs: number;
    percentageMetric: number;
  };
  rpo: {
    targetMs: number;
    currentMs: number;
    percentageMetric: number;
  };
  tier: 'CRITICAL' | 'HIGH' | 'MEDIUM' | 'LOW';
  sla: number; // 99.99, 99.95, etc
}

// Restore Job Configuration
export interface RestoreJobConfig {
  restoreId: string;
  backupId: string;
  sourceRegion: string;
  targetRegion: string;
  targetDatabase: string;
  restoreTime?: Date; // For PITR
  verifyAfterRestore: boolean;
  parallelism: number;
  timeout: number; // milliseconds
}

// Restore Job Status
export interface RestoreJobStatus {
  restoreId: string;
  backupId: string;
  status: 'PENDING' | 'IN_PROGRESS' | 'COMPLETED' | 'FAILED' | 'CANCELLED';
  progress: number; // 0-100
  startTime: Date;
  endTime?: Date;
  estimatedTimeRemaining: number;
  bytesRestored: number;
  totalBytes: number;
  errors: RestoreError[];
  warnings: string[];
}

// Recovery Error Details
export interface RestoreError {
  code: string;
  message: string;
  details?: string;
  timestamp: Date;
  recoverable: boolean;
}

// Backup Schedule Configuration
export interface BackupSchedule {
  scheduleId: string;
  name: string;
  fullBackupCron: string; // e.g., "0 2 * * 0" (Weekly Sunday 2 AM)
  incrementalBackupCron: string; // e.g., "0 * * * *" (Hourly)
  retentionPolicy: {
    fullBackups: number;
    incrementalBackups: number;
    dailyBackups: number;
    weeklyBackups: number;
    monthlyBackups: number;
  };
  regions: string[];
  enabled: boolean;
  notificationEmail: string;
}

// Disaster Recovery Test Result
export interface DRTestResult {
  testId: string;
  timestamp: Date;
  backupUsed: string;
  restoreTimeMs: number;
  verificationStatus: 'PASSED' | 'FAILED';
  dataIntegrityCheck: {
    passed: boolean;
    rowsValidated: number;
    mismatches: number;
  };
  reportUrl: string;
}

// Replication Stream Configuration
export interface ReplicationStream {
  streamId: string;
  sourceRegion: string;
  targetRegion: string;
  status: 'ACTIVE' | 'PAUSED' | 'STOPPED' | 'ERROR';
  bytesReplicated: number;
  lastEventTime: Date;
  lagMs: number;
  throughputMbps: number;
}

// Backup Storage Backend
export interface BackupStorageBackend {
  type: 'S3' | 'GCS' | 'AZURE_BLOB' | 'LOCAL';
  bucket: string;
  region?: string;
  accessKey?: string;
  secretKey?: string;
  encryption: {
    enabled: boolean;
    algorithm: string;
    keyArn?: string;
  };
}

// Monitoring and Metrics
export interface BackupMetrics {
  timestamp: Date;
  lastBackupTime: Date;
  lastBackupSize: number;
  lastBackupDuration: number;
  successRate: number; // percentage
  failureRate: number;
  averageRPO: number; // milliseconds
  averageRTO: number; // milliseconds
  totalBackupsStored: number;
  totalStorageUsed: number;
  estimatedRestoreTime: number;
  replicationStatus: ReplicaStatus[];
}

// Audit and Compliance
export interface BackupAuditLog {
  auditId: string;
  timestamp: Date;
  action: 'BACKUP_START' | 'BACKUP_COMPLETE' | 'RESTORE_START' | 'RESTORE_COMPLETE' | 'REPLICATION_SYNC' | 'VALIDATION_RUN';
  backupId?: string;
  restoreId?: string;
  user: string;
  result: 'SUCCESS' | 'FAILURE';
  details: string;
}

// Health Check Result
export interface BackupHealthCheck {
  timestamp: Date;
  lastBackupTime: Date;
  lastBackupSuccessful: boolean;
  replicationHealthy: boolean;
  pitrCapable: boolean;
  rtoMetTarget: boolean;
  rpoMetTarget: boolean;
  storageAvailable: boolean;
  issues: HealthIssue[];
}

export interface HealthIssue {
  severity: 'CRITICAL' | 'WARNING' | 'INFO';
  component: string;
  message: string;
  recommendation: string;
}
