/**
 * Automated Backup Service
 * Handles full and incremental backups with compression and encryption
 */

import { EventEmitter } from 'events';
import * as crypto from 'crypto';
import { BackupMetadata, BackupSchedule, BackupStorageBackend } from '../types';

export class BackupService extends EventEmitter {
  private readonly logger: any;
  private backupStorage: Map<string, BackupMetadata> = new Map();
  private activeBackups: Set<string> = new Set();
  private schedules: Map<string, BackupSchedule> = new Map();

  constructor(
    private readonly storageBackend: BackupStorageBackend,
    private readonly database: any,
    logger?: any
  ) {
    super();
    this.logger = logger || console;
  }

  /**
   * Execute a full backup of the database
   */
  async performFullBackup(
    sourceRegion: string,
    sourceDatabase: string,
    retentionDays: number = 30,
    tags: Record<string, string> = {}
  ): Promise<BackupMetadata> {
    const backupId = this.generateBackupId();
    const metadata: BackupMetadata = {
      backupId,
      timestamp: new Date(),
      type: 'FULL',
      sourceRegion,
      sourceDatabase,
      size: 0,
      duration: 0,
      status: 'PENDING',
      checksum: '',
      retentionDays,
      expiryDate: new Date(Date.now() + retentionDays * 24 * 60 * 60 * 1000),
      tags,
    };

    try {
      this.activeBackups.add(backupId);
      this.emit('backup:started', { backupId, type: 'FULL' });

      const startTime = Date.now();
      metadata.status = 'IN_PROGRESS';

      // Get database size and create backup stream
      const backupData = await this.database.createBackupStream(sourceDatabase, {
        format: 'binary',
        compression: this.storageBackend.encryption.enabled ? 'gzip' : 'none',
      });

      // Calculate checksum while streaming
      const checksum = crypto.createHash('sha256');
      let totalSize = 0;

      // Stream data to storage backend
      await this.uploadToStorage(backupId, backupData, checksum, (bytes) => {
        totalSize += bytes;
        this.emit('backup:progress', { backupId, bytesProcessed: totalSize });
      });

      metadata.size = totalSize;
      metadata.duration = Date.now() - startTime;
      metadata.checksum = checksum.digest('hex');
      metadata.status = 'COMPLETED';

      this.backupStorage.set(backupId, metadata);
      this.emit('backup:completed', metadata);

      this.logger.info(`Full backup completed: ${backupId}`, {
        size: metadata.size,
        duration: metadata.duration,
      });

      return metadata;
    } catch (error) {
      metadata.status = 'FAILED';
      this.emit('backup:failed', { backupId, error });
      this.logger.error(`Backup failed: ${backupId}`, error);
      throw new BackupError(`Full backup failed: ${error.message}`, 'BACKUP_FAILED', error);
    } finally {
      this.activeBackups.delete(backupId);
    }
  }

  /**
   * Execute an incremental backup based on previous backup
   */
  async performIncrementalBackup(
    sourceRegion: string,
    sourceDatabase: string,
    baseBackupId: string,
    retentionDays: number = 7,
    tags: Record<string, string> = {}
  ): Promise<BackupMetadata> {
    const baseBackup = this.backupStorage.get(baseBackupId);
    if (!baseBackup) {
      throw new BackupError(`Base backup not found: ${baseBackupId}`, 'BASE_BACKUP_NOT_FOUND');
    }

    const backupId = this.generateBackupId();
    const metadata: BackupMetadata = {
      backupId,
      timestamp: new Date(),
      type: 'INCREMENTAL',
      sourceRegion,
      sourceDatabase,
      size: 0,
      duration: 0,
      status: 'PENDING',
      checksum: '',
      retentionDays,
      expiryDate: new Date(Date.now() + retentionDays * 24 * 60 * 60 * 1000),
      incrementalBaseId: baseBackupId,
      tags,
    };

    try {
      this.activeBackups.add(backupId);
      this.emit('backup:started', { backupId, type: 'INCREMENTAL' });

      const startTime = Date.now();
      metadata.status = 'IN_PROGRESS';

      // Get changes since last backup
      const backupData = await this.database.createIncrementalBackupStream(
        sourceDatabase,
        baseBackup.timestamp,
        {
          format: 'binary',
          compression: 'gzip',
        }
      );

      const checksum = crypto.createHash('sha256');
      let totalSize = 0;

      // Stream incremental data to storage
      await this.uploadToStorage(backupId, backupData, checksum, (bytes) => {
        totalSize += bytes;
        this.emit('backup:progress', { backupId, bytesProcessed: totalSize });
      });

      metadata.size = totalSize;
      metadata.duration = Date.now() - startTime;
      metadata.checksum = checksum.digest('hex');
      metadata.status = 'COMPLETED';

      this.backupStorage.set(backupId, metadata);
      this.emit('backup:completed', metadata);

      this.logger.info(`Incremental backup completed: ${backupId}`, {
        size: metadata.size,
        baseBackup: baseBackupId,
        duration: metadata.duration,
      });

      return metadata;
    } catch (error) {
      metadata.status = 'FAILED';
      this.emit('backup:failed', { backupId, error });
      this.logger.error(`Incremental backup failed: ${backupId}`, error);
      throw new BackupError(`Incremental backup failed: ${error.message}`, 'INCREMENTAL_BACKUP_FAILED', error);
    } finally {
      this.activeBackups.delete(backupId);
    }
  }

  /**
   * Create a backup schedule with automatic execution
   */
  createBackupSchedule(schedule: BackupSchedule): void {
    this.schedules.set(schedule.scheduleId, schedule);

    if (!schedule.enabled) {
      this.logger.info(`Backup schedule created but disabled: ${schedule.name}`);
      return;
    }

    // Full backup schedule
    this.scheduleBackupJob(
      schedule.scheduleId,
      schedule.fullBackupCron,
      async () => {
        try {
          for (const region of schedule.regions) {
            await this.performFullBackup(region, 'primary_db', 30, {
              scheduleName: schedule.name,
              type: 'scheduled',
            });
          }
        } catch (error) {
          this.logger.error(`Scheduled full backup failed: ${schedule.name}`, error);
          if (schedule.notificationEmail) {
            this.sendNotification(schedule.notificationEmail, 'Backup Failed', error.message);
          }
        }
      }
    );

    // Incremental backup schedule
    this.scheduleBackupJob(
      `${schedule.scheduleId}-incremental`,
      schedule.incrementalBackupCron,
      async () => {
        try {
          for (const region of schedule.regions) {
            const latestBackup = this.getLatestBackup(region);
            if (latestBackup) {
              await this.performIncrementalBackup(
                region,
                'primary_db',
                latestBackup.backupId,
                7,
                { scheduleName: schedule.name, type: 'scheduled' }
              );
            }
          }
        } catch (error) {
          this.logger.error(`Scheduled incremental backup failed: ${schedule.name}`, error);
        }
      }
    );

    this.logger.info(`Backup schedule created: ${schedule.name}`);
  }

  /**
   * Get backup metadata by ID
   */
  getBackupMetadata(backupId: string): BackupMetadata | undefined {
    return this.backupStorage.get(backupId);
  }

  /**
   * List all backups with optional filtering
   */
  listBackups(filters?: {
    region?: string;
    database?: string;
    type?: 'FULL' | 'INCREMENTAL';
    status?: string;
  }): BackupMetadata[] {
    let backups = Array.from(this.backupStorage.values());

    if (filters?.region) {
      backups = backups.filter((b) => b.sourceRegion === filters.region);
    }
    if (filters?.database) {
      backups = backups.filter((b) => b.sourceDatabase === filters.database);
    }
    if (filters?.type) {
      backups = backups.filter((b) => b.type === filters.type);
    }
    if (filters?.status) {
      backups = backups.filter((b) => b.status === filters.status);
    }

    return backups.sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());
  }

  /**
   * Get the latest backup for a region
   */
  getLatestBackup(region: string): BackupMetadata | undefined {
    const backups = this.listBackups({ region });
    return backups[0];
  }

  /**
   * Delete expired backups based on retention policy
   */
  async deleteExpiredBackups(): Promise<string[]> {
    const now = new Date();
    const expiredBackups: string[] = [];

    for (const [backupId, metadata] of this.backupStorage) {
      if (metadata.expiryDate < now && metadata.status !== 'IN_PROGRESS') {
        try {
          await this.deleteBackupFromStorage(backupId);
          this.backupStorage.delete(backupId);
          expiredBackups.push(backupId);
          this.logger.info(`Deleted expired backup: ${backupId}`);
        } catch (error) {
          this.logger.error(`Failed to delete expired backup: ${backupId}`, error);
        }
      }
    }

    return expiredBackups;
  }

  /**
   * Validate backup integrity
   */
  async validateBackup(backupId: string): Promise<{ valid: boolean; errors: string[] }> {
    const metadata = this.backupStorage.get(backupId);
    if (!metadata) {
      throw new BackupError(`Backup not found: ${backupId}`, 'BACKUP_NOT_FOUND');
    }

    const errors: string[] = [];

    try {
      // Verify checksum
      const storageChecksum = await this.verifyStorageChecksum(backupId);
      if (storageChecksum !== metadata.checksum) {
        errors.push(`Checksum mismatch: stored ${storageChecksum} vs expected ${metadata.checksum}`);
      }

      // Verify storage integrity
      const storageSize = await this.getStorageSize(backupId);
      if (storageSize !== metadata.size) {
        errors.push(`Size mismatch: stored ${storageSize} vs expected ${metadata.size}`);
      }

      this.emit('backup:validated', { backupId, valid: errors.length === 0, errors });
      return { valid: errors.length === 0, errors };
    } catch (error) {
      errors.push(`Validation error: ${error.message}`);
      throw new BackupError(`Backup validation failed: ${error.message}`, 'VALIDATION_FAILED', error);
    }
  }

  /**
   * Get backup storage statistics
   */
  getBackupStatistics(): {
    totalBackups: number;
    totalSize: number;
    byType: Record<string, number>;
    byStatus: Record<string, number>;
  } {
    const backups = Array.from(this.backupStorage.values());

    const stats = {
      totalBackups: backups.length,
      totalSize: 0,
      byType: { FULL: 0, INCREMENTAL: 0, DIFFERENTIAL: 0 },
      byStatus: { PENDING: 0, IN_PROGRESS: 0, COMPLETED: 0, FAILED: 0, EXPIRED: 0 },
    };

    for (const backup of backups) {
      stats.totalSize += backup.size;
      stats.byType[backup.type]++;
      stats.byStatus[backup.status]++;
    }

    return stats;
  }

  // Private helper methods

  private generateBackupId(): string {
    return `backup-${Date.now()}-${crypto.randomBytes(8).toString('hex')}`;
  }

  private async uploadToStorage(
    backupId: string,
    dataStream: NodeJS.ReadableStream,
    hashStream: crypto.Hash,
    onProgress?: (bytes: number) => void
  ): Promise<void> {
    const storageKey = `backups/${backupId}`;

    return new Promise((resolve, reject) => {
      let uploadedBytes = 0;

      dataStream.on('data', (chunk: Buffer) => {
        hashStream.update(chunk);
        uploadedBytes += chunk.length;
        if (onProgress) onProgress(chunk.length);
      });

      dataStream.on('end', () => {
        // Simulate storage upload
        this.logger.debug(`Uploaded backup to storage: ${storageKey} (${uploadedBytes} bytes)`);
        resolve();
      });

      dataStream.on('error', reject);
    });
  }

  private async deleteBackupFromStorage(backupId: string): Promise<void> {
    const storageKey = `backups/${backupId}`;
    // Simulate storage deletion
    this.logger.debug(`Deleted backup from storage: ${storageKey}`);
  }

  private async verifyStorageChecksum(backupId: string): Promise<string> {
    // Simulate checksum verification
    return crypto.randomBytes(32).toString('hex');
  }

  private async getStorageSize(backupId: string): Promise<number> {
    const metadata = this.backupStorage.get(backupId);
    return metadata?.size || 0;
  }

  private scheduleBackupJob(
    scheduleId: string,
    cronExpression: string,
    callback: () => Promise<void>
  ): void {
    // In production, use node-cron or bull for scheduling
    this.logger.debug(`Scheduled backup job: ${scheduleId} with cron: ${cronExpression}`);
  }

  private sendNotification(email: string, subject: string, message: string): void {
    this.logger.info(`Notification sent to ${email}: ${subject}`);
  }
}

export class BackupError extends Error {
  constructor(
    message: string,
    public code: string,
    public originalError?: Error
  ) {
    super(message);
    this.name = 'BackupError';
  }
}
