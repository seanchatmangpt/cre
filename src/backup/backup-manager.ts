/**
 * Backup Manager
 * Main orchestration layer for backup, replication, and recovery
 */

import { EventEmitter } from 'events';
import { BackupService } from './services/backup.service';
import { ReplicationEngine } from './replication/replication.engine';
import { RecoveryService } from './recovery/recovery.service';
import { MonitoringService } from './monitoring/monitoring.service';
import {
  ReplicationConfig,
  PITRConfig,
  BackupSchedule,
  RestoreJobConfig,
  RTORPOGuarantees,
  BackupHealthCheck,
} from './types';

export class BackupManager extends EventEmitter {
  private backupService: BackupService;
  private replicationEngine: ReplicationEngine;
  private recoveryService: RecoveryService;
  private monitoringService: MonitoringService;
  private logger: any;

  constructor(
    private readonly database: any,
    private readonly storageBackend: any,
    private readonly replicationConfig: ReplicationConfig,
    private readonly pitrConfig: PITRConfig,
    logger?: any
  ) {
    super();
    this.logger = logger || console;

    // Initialize all services
    this.backupService = new BackupService(storageBackend, database, this.logger);
    this.replicationEngine = new ReplicationEngine(replicationConfig, database, storageBackend, this.logger);
    this.recoveryService = new RecoveryService(
      pitrConfig,
      database,
      storageBackend,
      (id) => this.backupService.getBackupMetadata(id),
      this.logger
    );
    this.monitoringService = new MonitoringService(
      this.backupService,
      this.replicationEngine,
      this.recoveryService,
      this.logger
    );

    this.setupEventListeners();
  }

  /**
   * Initialize backup system and start automated processes
   */
  async initialize(): Promise<void> {
    this.logger.info('Initializing backup and disaster recovery system');

    try {
      // Perform health check
      const healthCheck = await this.monitoringService.performHealthCheck();
      this.emit('initialized', { healthCheck });

      // Start monitoring
      this.startMonitoring();

      this.logger.info('Backup system initialized successfully');
    } catch (error) {
      this.logger.error('Failed to initialize backup system', error);
      throw error;
    }
  }

  /**
   * Create and execute a backup schedule
   */
  createBackupSchedule(schedule: BackupSchedule): void {
    this.logger.info(`Creating backup schedule: ${schedule.name}`);
    this.backupService.createBackupSchedule(schedule);
    this.monitoringService.recordAuditLog(
      'BACKUP_START',
      'system',
      'SUCCESS',
      `Backup schedule created: ${schedule.name}`
    );
  }

  /**
   * Execute immediate full backup
   */
  async executeFullBackup(sourceRegion: string, sourceDatabase: string = 'primary_db'): Promise<string> {
    this.logger.info(`Executing full backup for region: ${sourceRegion}`);

    const startTime = Date.now();
    try {
      const backup = await this.backupService.performFullBackup(sourceRegion, sourceDatabase);

      // Start replication to secondary regions
      await this.replicationEngine.startReplication(backup.backupId);

      this.monitoringService.recordAuditLog(
        'BACKUP_COMPLETE',
        'system',
        'SUCCESS',
        `Full backup completed: ${backup.backupId}`,
        backup.backupId
      );

      this.emit('backup:full-completed', {
        backupId: backup.backupId,
        size: backup.size,
        duration: Date.now() - startTime,
      });

      return backup.backupId;
    } catch (error) {
      this.monitoringService.recordAuditLog(
        'BACKUP_COMPLETE',
        'system',
        'FAILURE',
        `Backup failed: ${error.message}`
      );
      throw error;
    }
  }

  /**
   * Execute immediate incremental backup
   */
  async executeIncrementalBackup(sourceRegion: string): Promise<string> {
    this.logger.info(`Executing incremental backup for region: ${sourceRegion}`);

    const startTime = Date.now();
    try {
      const latestBackup = this.backupService.getLatestBackup(sourceRegion);
      if (!latestBackup) {
        throw new Error('No base backup found for incremental backup');
      }

      const backup = await this.backupService.performIncrementalBackup(
        sourceRegion,
        'primary_db',
        latestBackup.backupId
      );

      // Replicate incremental backup
      await this.replicationEngine.startReplication(backup.backupId);

      this.monitoringService.recordAuditLog(
        'BACKUP_COMPLETE',
        'system',
        'SUCCESS',
        `Incremental backup completed: ${backup.backupId}`,
        backup.backupId
      );

      return backup.backupId;
    } catch (error) {
      this.monitoringService.recordAuditLog(
        'BACKUP_COMPLETE',
        'system',
        'FAILURE',
        `Incremental backup failed: ${error.message}`
      );
      throw error;
    }
  }

  /**
   * Initiate a restore operation
   */
  async initiateRestore(config: RestoreJobConfig): Promise<string> {
    this.logger.info(`Initiating restore: ${config.restoreId}`);

    try {
      const restoreStatus = await this.recoveryService.initiateRestore(config);

      this.monitoringService.recordAuditLog(
        'RESTORE_COMPLETE',
        'system',
        'SUCCESS',
        `Restore completed: ${config.restoreId}`,
        config.backupId,
        config.restoreId
      );

      this.emit('restore:initiated', {
        restoreId: config.restoreId,
        backupId: config.backupId,
      });

      return config.restoreId;
    } catch (error) {
      this.monitoringService.recordAuditLog(
        'RESTORE_COMPLETE',
        'system',
        'FAILURE',
        `Restore failed: ${error.message}`
      );
      throw error;
    }
  }

  /**
   * Perform point-in-time recovery
   */
  async performPITRecovery(
    sourceBackupId: string,
    targetRegion: string,
    restoreTime: Date
  ): Promise<string> {
    this.logger.info(`Performing PITR to ${restoreTime.toISOString()}`);

    try {
      const restoreStatus = await this.recoveryService.performPITRecovery(
        sourceBackupId,
        targetRegion,
        'recovered_db',
        restoreTime,
        true
      );

      this.monitoringService.recordAuditLog(
        'RESTORE_COMPLETE',
        'system',
        'SUCCESS',
        `PITR completed to: ${restoreTime.toISOString()}`,
        sourceBackupId,
        restoreStatus.restoreId
      );

      this.emit('pitr:completed', {
        restoreId: restoreStatus.restoreId,
        restoreTime,
      });

      return restoreStatus.restoreId;
    } catch (error) {
      this.monitoringService.recordAuditLog(
        'RESTORE_COMPLETE',
        'system',
        'FAILURE',
        `PITR failed: ${error.message}`
      );
      throw error;
    }
  }

  /**
   * Test disaster recovery by performing a restore to a test environment
   */
  async testDisasterRecovery(sourceBackupId: string, targetRegion: string): Promise<{
    testId: string;
    passed: boolean;
    duration: number;
    issues: string[];
  }> {
    const testId = `dr-test-${Date.now()}`;
    const startTime = Date.now();

    this.logger.info(`Starting disaster recovery test: ${testId}`);

    try {
      const restoreConfig: RestoreJobConfig = {
        restoreId: testId,
        backupId: sourceBackupId,
        sourceRegion: this.replicationConfig.primaryRegion,
        targetRegion,
        targetDatabase: 'dr_test_db',
        verifyAfterRestore: true,
        parallelism: 4,
        timeout: 7200000, // 2 hours
      };

      const restoreStatus = await this.recoveryService.initiateRestore(restoreConfig);
      const validation = await this.recoveryService.validateRestoration(testId, 'dr_test_db');

      const duration = Date.now() - startTime;
      const passed = validation.valid && restoreStatus.status === 'COMPLETED';

      this.monitoringService.recordAuditLog(
        'BACKUP_COMPLETE',
        'system',
        passed ? 'SUCCESS' : 'FAILURE',
        `Disaster recovery test ${passed ? 'passed' : 'failed'}: ${testId}`
      );

      this.emit('dr-test:completed', {
        testId,
        passed,
        duration,
        issues: validation.issues,
      });

      return {
        testId,
        passed,
        duration,
        issues: validation.issues,
      };
    } catch (error) {
      this.logger.error(`Disaster recovery test failed: ${testId}`, error);
      throw error;
    }
  }

  /**
   * Get current RTO/RPO status
   */
  getRTORPOStatus(): RTORPOGuarantees {
    return this.monitoringService.getRTORPOStatus();
  }

  /**
   * Perform system health check
   */
  async performHealthCheck(): Promise<BackupHealthCheck> {
    return this.monitoringService.performHealthCheck();
  }

  /**
   * Get list of available restore points for PITR
   */
  getRestorePoints(region: string): Date[] {
    return this.recoveryService.getRestorePoints(region);
  }

  /**
   * Get replication status for all regions
   */
  getReplicationStatus() {
    return this.replicationEngine.getReplicationStatus();
  }

  /**
   * Get comprehensive metrics report
   */
  generateMetricsReport(days: number = 7) {
    return this.monitoringService.generateMetricsReport(days);
  }

  /**
   * Cleanup system and stop all monitoring
   */
  async shutdown(): Promise<void> {
    this.logger.info('Shutting down backup system');

    this.replicationEngine.stopMonitoring();
    this.removeAllListeners();

    this.logger.info('Backup system shutdown complete');
  }

  /**
   * List all backups
   */
  listBackups(filters?: any) {
    return this.backupService.listBackups(filters);
  }

  /**
   * List all restore jobs
   */
  listRestoreJobs(filters?: any) {
    return this.recoveryService.listRestoreJobs(filters);
  }

  /**
   * Get audit logs
   */
  getAuditLogs(filters?: any) {
    return this.monitoringService.getAuditLogs(filters);
  }

  // Private helper methods

  private setupEventListeners(): void {
    // Forward events from sub-services
    this.backupService.on('backup:started', (data) => this.emit('backup:started', data));
    this.backupService.on('backup:completed', (data) => this.emit('backup:completed', data));
    this.backupService.on('backup:failed', (data) => this.emit('backup:failed', data));

    this.replicationEngine.on('replication:started', (data) => this.emit('replication:started', data));
    this.replicationEngine.on('replication:failed', (data) => this.emit('replication:failed', data));
    this.replicationEngine.on('replication:rpo-violation', (data) => this.emit('replication:rpo-violation', data));

    this.recoveryService.on('restore:initiated', (data) => this.emit('restore:initiated', data));
    this.recoveryService.on('restore:completed', (data) => this.emit('restore:completed', data));
    this.recoveryService.on('restore:failed', (data) => this.emit('restore:failed', data));

    this.monitoringService.on('alert:rto-warning', (data) => this.emit('alert:rto-warning', data));
    this.monitoringService.on('alert:rpo-critical', (data) => this.emit('alert:rpo-critical', data));
    this.monitoringService.on('audit:logged', (data) => this.emit('audit:logged', data));
  }

  private startMonitoring(): void {
    // Record metrics every minute
    setInterval(() => {
      const stats = this.backupService.getBackupStatistics();
      const replicationStatus = this.replicationEngine.getReplicationStatus();
      const recoveryMetrics = this.recoveryService.getRecoveryMetrics();

      this.monitoringService.recordBackupMetrics({
        lastBackupTime: new Date(),
        lastBackupSize: stats.totalSize,
        lastBackupDuration: 0,
        successRate: 0.95,
        failureRate: 0.05,
        averageRPO: 300000,
        averageRTO: 3600000,
        totalBackupsStored: stats.totalBackups,
        totalStorageUsed: stats.totalSize,
        estimatedRestoreTime: recoveryMetrics.averageRTO,
        replicationStatus,
      });
    }, 60000);

    // Perform health check every 5 minutes
    setInterval(async () => {
      try {
        await this.monitoringService.performHealthCheck();
      } catch (error) {
        this.logger.error('Health check failed', error);
      }
    }, 300000);

    // Delete expired backups every hour
    setInterval(async () => {
      try {
        const deleted = await this.backupService.deleteExpiredBackups();
        if (deleted.length > 0) {
          this.logger.info(`Deleted ${deleted.length} expired backups`);
        }
      } catch (error) {
        this.logger.error('Failed to delete expired backups', error);
      }
    }, 3600000);
  }
}

export { BackupService, ReplicationEngine, RecoveryService, MonitoringService };
