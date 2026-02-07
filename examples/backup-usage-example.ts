/**
 * Backup and Disaster Recovery System - Usage Examples
 */

import { BackupManager } from '../src/backup/backup-manager';
import {
  ReplicationConfig,
  PITRConfig,
  BackupSchedule,
  RestoreJobConfig,
} from '../src/backup/types';

/**
 * Example 1: Initialize the backup system with multi-region replication
 */
async function initializeBackupSystem() {
  console.log('=== Initializing Backup System ===\n');

  const database = require('./db-connection'); // Your database connection
  const storageBackend = {
    type: 'S3',
    bucket: 'my-database-backups',
    region: 'us-east-1',
    encryption: { enabled: true, algorithm: 'AES-256', keyArn: 'arn:aws:kms:...' },
  };

  // Configure multi-region replication for RTO/RPO guarantees
  const replicationConfig: ReplicationConfig = {
    primaryRegion: 'us-east-1',
    secondaryRegions: ['us-west-2', 'eu-west-1', 'ap-southeast-1'],
    replicationMode: 'ASYNCHRONOUS', // Use SYNCHRONOUS for 0 RPO
    maxReplicationLagMs: 300000, // 5 minutes RPO
    targetRTOMs: 3600000, // 1 hour RTO
    crossRegionBandwidthMbps: 500,
    compressionLevel: 'HIGH',
    encryptionEnabled: true,
    validationEnabled: true,
  };

  // Configure point-in-time recovery
  const pitrConfig: PITRConfig = {
    enabled: true,
    retentionHours: 72, // 72 hours of PITR capability
    snapshotIntervalMs: 300000, // Snapshot every 5 minutes
    transactionLogRetentionDays: 7,
    minRestorableTime: new Date(Date.now() - 72 * 60 * 60 * 1000),
    maxRestorableTime: new Date(),
  };

  const backupManager = new BackupManager(
    database,
    storageBackend,
    replicationConfig,
    pitrConfig,
    console // logger
  );

  await backupManager.initialize();
  console.log('✓ Backup system initialized\n');

  return backupManager;
}

/**
 * Example 2: Create automated backup schedules
 */
async function setupBackupSchedules(backupManager: BackupManager) {
  console.log('=== Setting Up Backup Schedules ===\n');

  // Schedule full backup every Sunday at 2 AM
  const fullBackupSchedule: BackupSchedule = {
    scheduleId: 'full-backup-weekly',
    name: 'Weekly Full Backup',
    fullBackupCron: '0 2 * * 0', // Every Sunday at 2:00 AM
    incrementalBackupCron: '0 * * * *', // Every hour
    retentionPolicy: {
      fullBackups: 5, // Keep last 5 full backups
      incrementalBackups: 168, // Keep 1 week of incremental (hourly)
      dailyBackups: 7,
      weeklyBackups: 4,
      monthlyBackups: 12,
    },
    regions: ['us-east-1', 'us-west-2'],
    enabled: true,
    notificationEmail: 'dba@company.com',
  };

  backupManager.createBackupSchedule(fullBackupSchedule);
  console.log('✓ Full backup schedule created: Every Sunday at 2:00 AM\n');

  // Listen to backup completion events
  backupManager.on('backup:completed', (data) => {
    console.log(`Backup completed: ${data.backupId}`);
    console.log(`  Size: ${(data.size / 1024 / 1024 / 1024).toFixed(2)} GB`);
    console.log(`  Duration: ${(data.duration / 1000 / 60).toFixed(2)} minutes`);
  });

  backupManager.on('backup:failed', (data) => {
    console.error(`Backup failed: ${data.error.message}`);
  });
}

/**
 * Example 3: Execute on-demand backups
 */
async function executeOnDemandBackup(backupManager: BackupManager) {
  console.log('=== Executing On-Demand Full Backup ===\n');

  try {
    const backupId = await backupManager.executeFullBackup('us-east-1');
    console.log(`✓ Full backup created: ${backupId}`);

    const backups = backupManager.listBackups({ region: 'us-east-1' });
    console.log(`\nBackup statistics:`);
    console.log(`  Total backups: ${backups.length}`);
    console.log(`  Total size: ${(backups.reduce((sum, b) => sum + b.size, 0) / 1024 / 1024 / 1024).toFixed(2)} GB`);
  } catch (error) {
    console.error('Backup failed:', error.message);
  }
}

/**
 * Example 4: Monitor replication status
 */
async function monitorReplication(backupManager: BackupManager) {
  console.log('=== Monitoring Replication Status ===\n');

  const replicationStatus = backupManager.getReplicationStatus();

  console.log('Regional Replicas:');
  for (const replica of replicationStatus) {
    const status = replica.status === 'HEALTHY' ? '✓' : '✗';
    console.log(`  ${status} ${replica.region}`);
    console.log(`    Status: ${replica.status}`);
    console.log(`    Lag: ${replica.lagMs}ms`);
    console.log(`    Size: ${(replica.dataSize / 1024 / 1024).toFixed(2)} MB`);
    console.log(`    Last Sync: ${replica.lastSyncTime.toISOString()}`);
  }

  // Check RPO compliance
  const rtorpoStatus = backupManager.getRTORPOStatus();
  console.log(`\nRPO Status:`);
  console.log(`  Target: ${rtorpoStatus.rpo.targetMs}ms`);
  console.log(`  Current: ${rtorpoStatus.rpo.currentMs}ms`);
  console.log(`  Compliance: ${rtorpoStatus.rpo.percentageMetric.toFixed(1)}%`);
}

/**
 * Example 5: Perform point-in-time recovery
 */
async function performPITRecovery(backupManager: BackupManager) {
  console.log('=== Performing Point-in-Time Recovery ===\n');

  try {
    // Get list of available restore points
    const restorePoints = backupManager.getRestorePoints('us-east-1');
    console.log(`Available restore points: ${restorePoints.length}`);
    console.log(`  Earliest: ${restorePoints[0].toISOString()}`);
    console.log(`  Latest: ${restorePoints[restorePoints.length - 1].toISOString()}`);

    // Recover to 1 hour ago
    const restoreTime = new Date(Date.now() - 60 * 60 * 1000);
    console.log(`\nRecovering to: ${restoreTime.toISOString()}`);

    // Get the latest backup
    const backups = backupManager.listBackups({ region: 'us-east-1' });
    if (backups.length === 0) {
      throw new Error('No backups available');
    }

    const latestBackup = backups[0];
    const restoreId = await backupManager.performPITRecovery(
      latestBackup.backupId,
      'us-west-2', // Restore to different region
      restoreTime
    );

    console.log(`✓ PITR restore initiated: ${restoreId}`);

    // Monitor restore progress
    backupManager.on('restore:progress', (data) => {
      const progress = data.progress;
      const bytes = data.bytesRestored / data.totalBytes;
      console.log(`Restore progress: ${progress}% (${(bytes * 100).toFixed(1)}% data)`);
    });
  } catch (error) {
    console.error('PITR failed:', error.message);
  }
}

/**
 * Example 6: Restore from a specific backup
 */
async function restoreFromBackup(backupManager: BackupManager, backupId: string) {
  console.log('=== Restoring from Backup ===\n');

  const config: RestoreJobConfig = {
    restoreId: `restore-${Date.now()}`,
    backupId,
    sourceRegion: 'us-east-1',
    targetRegion: 'us-west-2',
    targetDatabase: 'restored_production_db',
    verifyAfterRestore: true,
    parallelism: 8, // Use 8 parallel streams
    timeout: 3600000, // 1 hour timeout
  };

  try {
    const restoreId = await backupManager.initiateRestore(config);
    console.log(`✓ Restore job created: ${restoreId}`);
    console.log(`  Backup: ${backupId}`);
    console.log(`  Source Region: us-east-1`);
    console.log(`  Target Region: us-west-2`);
    console.log(`  Target Database: restored_production_db`);
    console.log(`  Parallelism: 8 streams`);
  } catch (error) {
    console.error('Restore failed:', error.message);
  }
}

/**
 * Example 7: Monitor RTO/RPO guarantees
 */
async function monitorRTORPO(backupManager: BackupManager) {
  console.log('=== Monitoring RTO/RPO Guarantees ===\n');

  const rtorpoStatus = backupManager.getRTORPOStatus();

  console.log('Recovery Time Objective (RTO):');
  console.log(`  Target: ${(rtorpoStatus.rto.targetMs / 1000 / 60).toFixed(0)} minutes`);
  console.log(`  Current: ${(rtorpoStatus.rto.currentMs / 1000 / 60).toFixed(0)} minutes`);
  console.log(`  Compliance: ${rtorpoStatus.rto.percentageMetric.toFixed(1)}%`);

  console.log('\nRecovery Point Objective (RPO):');
  console.log(`  Target: ${(rtorpoStatus.rpo.targetMs / 1000).toFixed(0)} seconds`);
  console.log(`  Current: ${(rtorpoStatus.rpo.currentMs / 1000).toFixed(0)} seconds`);
  console.log(`  Compliance: ${rtorpoStatus.rpo.percentageMetric.toFixed(1)}%`);

  console.log(`\nSLA Tier: ${rtorpoStatus.tier}`);
  console.log(`SLA: ${rtorpoStatus.sla}%`);

  // Alert if not compliant
  if (rtorpoStatus.rto.percentageMetric < 80) {
    console.warn('⚠️  RTO target NOT met');
  }
  if (rtorpoStatus.rpo.percentageMetric < 80) {
    console.warn('⚠️  RPO target NOT met');
  }
}

/**
 * Example 8: Perform system health check
 */
async function performHealthCheck(backupManager: BackupManager) {
  console.log('=== System Health Check ===\n');

  const healthCheck = await backupManager.performHealthCheck();

  console.log(`Timestamp: ${healthCheck.timestamp.toISOString()}`);
  console.log(`Last Backup: ${healthCheck.lastBackupTime.toISOString()}`);
  console.log(`Last Backup Successful: ${healthCheck.lastBackupSuccessful ? '✓' : '✗'}`);
  console.log(`Replication Healthy: ${healthCheck.replicationHealthy ? '✓' : '✗'}`);
  console.log(`PITR Capable: ${healthCheck.pitrCapable ? '✓' : '✗'}`);
  console.log(`RTO Target Met: ${healthCheck.rtoMetTarget ? '✓' : '✗'}`);
  console.log(`RPO Target Met: ${healthCheck.rpoMetTarget ? '✓' : '✗'}`);
  console.log(`Storage Available: ${healthCheck.storageAvailable ? '✓' : '✗'}`);

  if (healthCheck.issues.length > 0) {
    console.log('\n⚠️  Issues Found:');
    for (const issue of healthCheck.issues) {
      console.log(`  [${issue.severity}] ${issue.component}: ${issue.message}`);
      console.log(`    Recommendation: ${issue.recommendation}`);
    }
  } else {
    console.log('\n✓ No issues found');
  }
}

/**
 * Example 9: Test disaster recovery
 */
async function testDisasterRecovery(backupManager: BackupManager) {
  console.log('=== Disaster Recovery Test ===\n');

  try {
    // First, create a backup
    const backupId = await backupManager.executeFullBackup('us-east-1');
    console.log(`Created test backup: ${backupId}`);

    // Perform DR test
    console.log('\nRunning DR test...');
    const drTest = await backupManager.testDisasterRecovery(backupId, 'us-west-2');

    console.log(`\n✓ DR Test Results:`);
    console.log(`  Test ID: ${drTest.testId}`);
    console.log(`  Status: ${drTest.passed ? 'PASSED' : 'FAILED'}`);
    console.log(`  Duration: ${(drTest.duration / 1000 / 60).toFixed(2)} minutes`);

    if (drTest.issues.length > 0) {
      console.log(`\n  Issues:`);
      for (const issue of drTest.issues) {
        console.log(`    - ${issue}`);
      }
    }
  } catch (error) {
    console.error('DR test failed:', error.message);
  }
}

/**
 * Example 10: Generate and view metrics report
 */
async function generateMetricsReport(backupManager: BackupManager) {
  console.log('=== Metrics Report (Last 7 Days) ===\n');

  const report = backupManager.generateMetricsReport(7);

  console.log(`Period: ${report.period.start.toISOString()} to ${report.period.end.toISOString()}`);
  console.log(`\nBackup Metrics:`);
  console.log(`  Total snapshots: ${report.backupMetrics.length}`);
  console.log(`  Average backup time: ${(report.averageBackupTime / 1000 / 60).toFixed(2)} minutes`);
  console.log(`  Success rate: ${report.successRate.toFixed(1)}%`);

  console.log(`\nRTO/RPO Status:`);
  console.log(`  RTO compliance: ${report.rtoStatus.rto.percentageMetric.toFixed(1)}%`);
  console.log(`  RPO compliance: ${report.rtoStatus.rpo.percentageMetric.toFixed(1)}%`);

  console.log(`\nRegional Replication:`);
  for (const status of report.replicationStatus) {
    console.log(`  ${status.region}: ${status.status} (Lag: ${status.lagMs}ms)`);
  }
}

/**
 * Example 11: View audit logs
 */
function viewAuditLogs(backupManager: BackupManager) {
  console.log('=== Audit Logs (Last 24 Hours) ===\n');

  const oneDayAgo = new Date(Date.now() - 24 * 60 * 60 * 1000);
  const logs = backupManager.getAuditLogs({
    startDate: oneDayAgo,
    limit: 20,
  });

  for (const log of logs) {
    const icon = log.result === 'SUCCESS' ? '✓' : '✗';
    console.log(
      `${icon} [${log.timestamp.toISOString()}] ${log.action} by ${log.user} - ${log.result}`
    );
    if (log.details) {
      console.log(`   ${log.details}`);
    }
  }
}

/**
 * Main execution
 */
async function main() {
  try {
    console.log('╔════════════════════════════════════════════════════════════════╗');
    console.log('║   Backup and Disaster Recovery System - Examples               ║');
    console.log('╚════════════════════════════════════════════════════════════════╝\n');

    const backupManager = await initializeBackupSystem();

    await setupBackupSchedules(backupManager);
    await executeOnDemandBackup(backupManager);
    await monitorReplication(backupManager);
    await monitorRTORPO(backupManager);
    await performHealthCheck(backupManager);
    await generateMetricsReport(backupManager);
    viewAuditLogs(backupManager);

    // Clean up
    await backupManager.shutdown();
    console.log('\n✓ System shutdown complete');
  } catch (error) {
    console.error('Fatal error:', error);
    process.exit(1);
  }
}

// Run examples
if (require.main === module) {
  main();
}

export {
  initializeBackupSystem,
  setupBackupSchedules,
  executeOnDemandBackup,
  monitorReplication,
  performPITRecovery,
  restoreFromBackup,
  monitorRTORPO,
  performHealthCheck,
  testDisasterRecovery,
  generateMetricsReport,
  viewAuditLogs,
};
