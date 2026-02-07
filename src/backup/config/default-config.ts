/**
 * Default Configuration for Backup and Disaster Recovery System
 */

import { ReplicationConfig, PITRConfig } from '../types';

/**
 * Standard RTO/RPO SLA Tiers
 */
export const SLATiers = {
  CRITICAL: {
    rtoMs: 900000, // 15 minutes
    rpoMs: 60000, // 1 minute
    replicationMode: 'SYNCHRONOUS',
    compressionLevel: 'LOW',
    description: 'Critical systems - minimal data loss',
  },
  HIGH: {
    rtoMs: 3600000, // 1 hour
    rpoMs: 300000, // 5 minutes
    replicationMode: 'ASYNCHRONOUS',
    compressionLevel: 'MEDIUM',
    description: 'High-priority systems',
  },
  STANDARD: {
    rtoMs: 7200000, // 2 hours
    rpoMs: 900000, // 15 minutes
    replicationMode: 'ASYNCHRONOUS',
    compressionLevel: 'HIGH',
    description: 'Standard production systems',
  },
  LOWER: {
    rtoMs: 86400000, // 24 hours
    rpoMs: 3600000, // 1 hour
    replicationMode: 'ASYNCHRONOUS',
    compressionLevel: 'HIGH',
    description: 'Non-critical systems',
  },
};

/**
 * Create replication config for critical tier (1 min RPO, 15 min RTO)
 */
export function createCriticalReplicationConfig(): ReplicationConfig {
  return {
    primaryRegion: 'us-east-1',
    secondaryRegions: ['us-west-2', 'eu-west-1', 'ap-southeast-1'],
    replicationMode: 'SYNCHRONOUS',
    maxReplicationLagMs: 60000, // 1 minute RPO
    targetRTOMs: 900000, // 15 minutes RTO
    crossRegionBandwidthMbps: 1000, // High bandwidth
    compressionLevel: 'LOW', // Low compression for speed
    encryptionEnabled: true,
    validationEnabled: true,
  };
}

/**
 * Create replication config for standard tier (5 min RPO, 1 hour RTO)
 */
export function createStandardReplicationConfig(
  primaryRegion: string = 'us-east-1',
  secondaryRegions: string[] = ['us-west-2', 'eu-west-1']
): ReplicationConfig {
  return {
    primaryRegion,
    secondaryRegions,
    replicationMode: 'ASYNCHRONOUS',
    maxReplicationLagMs: 300000, // 5 minutes RPO
    targetRTOMs: 3600000, // 1 hour RTO
    crossRegionBandwidthMbps: 500,
    compressionLevel: 'MEDIUM',
    encryptionEnabled: true,
    validationEnabled: true,
  };
}

/**
 * Create replication config for lower tier (1 hour RPO, 24 hour RTO)
 */
export function createLowerReplicationConfig(
  primaryRegion: string = 'us-east-1',
  secondaryRegions: string[] = ['us-west-2']
): ReplicationConfig {
  return {
    primaryRegion,
    secondaryRegions,
    replicationMode: 'ASYNCHRONOUS',
    maxReplicationLagMs: 3600000, // 1 hour RPO
    targetRTOMs: 86400000, // 24 hours RTO
    crossRegionBandwidthMbps: 100,
    compressionLevel: 'HIGH',
    encryptionEnabled: true,
    validationEnabled: false, // Skip validation for cost savings
  };
}

/**
 * Create PITR config with 72-hour retention
 */
export function createPITRConfig72h(): PITRConfig {
  const now = new Date();
  const seventyTwoHoursAgo = new Date(now.getTime() - 72 * 60 * 60 * 1000);

  return {
    enabled: true,
    retentionHours: 72,
    snapshotIntervalMs: 300000, // 5 minutes
    transactionLogRetentionDays: 7,
    minRestorableTime: seventyTwoHoursAgo,
    maxRestorableTime: now,
  };
}

/**
 * Create PITR config with 30-day retention
 */
export function createPITRConfig30d(): PITRConfig {
  const now = new Date();
  const thirtyDaysAgo = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000);

  return {
    enabled: true,
    retentionHours: 720, // 30 days
    snapshotIntervalMs: 3600000, // 1 hour
    transactionLogRetentionDays: 30,
    minRestorableTime: thirtyDaysAgo,
    maxRestorableTime: now,
  };
}

/**
 * Create PITR config with limited retention (24 hours)
 */
export function createPITRConfig24h(): PITRConfig {
  const now = new Date();
  const twentyFourHoursAgo = new Date(now.getTime() - 24 * 60 * 60 * 1000);

  return {
    enabled: true,
    retentionHours: 24,
    snapshotIntervalMs: 600000, // 10 minutes
    transactionLogRetentionDays: 3,
    minRestorableTime: twentyFourHoursAgo,
    maxRestorableTime: now,
  };
}

/**
 * Backup schedule presets
 */
export const BackupSchedulePresets = {
  AGGRESSIVE: {
    fullBackupCron: '0 2 * * *', // Daily at 2 AM
    incrementalBackupCron: '0 */4 * * *', // Every 4 hours
    retentionPolicy: {
      fullBackups: 30,
      incrementalBackups: 180,
      dailyBackups: 30,
      weeklyBackups: 12,
      monthlyBackups: 24,
    },
    description: 'Aggressive - Maximum coverage and retention',
  },
  STANDARD: {
    fullBackupCron: '0 2 * * 0', // Weekly Sunday 2 AM
    incrementalBackupCron: '0 * * * *', // Hourly
    retentionPolicy: {
      fullBackups: 5,
      incrementalBackups: 168,
      dailyBackups: 7,
      weeklyBackups: 4,
      monthlyBackups: 12,
    },
    description: 'Standard - Balanced coverage and cost',
  },
  LEAN: {
    fullBackupCron: '0 2 * * 0', // Weekly Sunday 2 AM
    incrementalBackupCron: '0 6 * * *', // Daily at 6 AM
    retentionPolicy: {
      fullBackups: 4,
      incrementalBackups: 7,
      dailyBackups: 7,
      weeklyBackups: 4,
      monthlyBackups: 3,
    },
    description: 'Lean - Minimum coverage for cost savings',
  },
};

/**
 * Storage backend configurations
 */
export const StorageBackendTemplates = {
  AWS_S3: {
    type: 'S3',
    region: 'us-east-1',
    bucket: 'my-database-backups',
    encryption: {
      enabled: true,
      algorithm: 'AES-256',
      keyArn: 'arn:aws:kms:us-east-1:123456789:key/12345678-1234-1234-1234-123456789012',
    },
  },
  GCP_GCS: {
    type: 'GCS',
    bucket: 'my-database-backups',
    encryption: {
      enabled: true,
      algorithm: 'AES-256',
    },
  },
  AZURE_BLOB: {
    type: 'AZURE_BLOB',
    bucket: 'my-database-backups',
    encryption: {
      enabled: true,
      algorithm: 'AES-256',
    },
  },
  LOCAL: {
    type: 'LOCAL',
    bucket: '/mnt/backups',
    encryption: {
      enabled: false,
      algorithm: 'NONE',
    },
  },
};

/**
 * Alert threshold templates
 */
export const AlertThresholdTemplates = {
  AGGRESSIVE: {
    rtoWarningMs: 1800000, // 30 minutes
    rpoCriticalMs: 120000, // 2 minutes
    backupFailureRate: 0.01, // 1%
    replicationLagMs: 300000, // 5 minutes
  },
  STANDARD: {
    rtoWarningMs: 3600000, // 1 hour
    rpoCriticalMs: 300000, // 5 minutes
    backupFailureRate: 0.05, // 5%
    replicationLagMs: 600000, // 10 minutes
  },
  RELAXED: {
    rtoWarningMs: 7200000, // 2 hours
    rpoCriticalMs: 900000, // 15 minutes
    backupFailureRate: 0.1, // 10%
    replicationLagMs: 1800000, // 30 minutes
  },
};

/**
 * Helper to merge custom config with defaults
 */
export function mergeConfig<T>(defaults: T, custom: Partial<T>): T {
  return { ...defaults, ...custom };
}

/**
 * Validate configuration
 */
export function validateReplicationConfig(config: ReplicationConfig): { valid: boolean; errors: string[] } {
  const errors: string[] = [];

  if (!config.primaryRegion) {
    errors.push('Primary region is required');
  }

  if (!config.secondaryRegions || config.secondaryRegions.length === 0) {
    errors.push('At least one secondary region is required');
  }

  if (config.maxReplicationLagMs <= 0) {
    errors.push('Max replication lag must be positive');
  }

  if (config.targetRTOMs <= 0) {
    errors.push('Target RTO must be positive');
  }

  if (config.crossRegionBandwidthMbps <= 0) {
    errors.push('Cross-region bandwidth must be positive');
  }

  if (
    config.replicationMode === 'SYNCHRONOUS' &&
    config.maxReplicationLagMs > config.targetRTOMs * 0.1
  ) {
    errors.push('Synchronous replication RPO should be < 10% of RTO');
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

export function validatePITRConfig(config: PITRConfig): { valid: boolean; errors: string[] } {
  const errors: string[] = [];

  if (config.enabled) {
    if (config.retentionHours <= 0) {
      errors.push('PITR retention hours must be positive');
    }

    if (config.snapshotIntervalMs <= 0) {
      errors.push('Snapshot interval must be positive');
    }

    if (config.transactionLogRetentionDays <= 0) {
      errors.push('Transaction log retention days must be positive');
    }

    if (config.minRestorableTime >= config.maxRestorableTime) {
      errors.push('Min restorable time must be before max restorable time');
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}
