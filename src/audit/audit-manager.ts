/**
 * Audit Event Manager
 * Manages audit event creation, queuing, and distribution to multiple backends
 */

import { v4 as uuidv4 } from 'uuid';
import {
  AuditEvent,
  AuditEventBatch,
  AuditEventType,
  AuditSeverity,
  AuditStatus,
  AuditEventFilter,
  AuditEventQueryResult,
  AuditLogConfig,
} from './types';
import { CloudLoggingBackend } from './backends/cloud-logging';
import { SiemExporter } from './backends/siem-exporter';
import { LocalAuditStore } from './backends/local-store';

export class AuditManager {
  private eventQueue: AuditEvent[] = [];
  private batchTimeout: NodeJS.Timeout | null = null;
  private cloudLogging: CloudLoggingBackend | null = null;
  private siemExporter: SiemExporter | null = null;
  private localStore: LocalAuditStore;
  private config: AuditLogConfig;

  constructor(config: AuditLogConfig) {
    this.config = config;
    this.localStore = new LocalAuditStore(config);

    if (config.enableCloudLogging) {
      this.cloudLogging = new CloudLoggingBackend(config);
    }

    if (config.enableSiemExport) {
      this.siemExporter = new SiemExporter(config);
    }
  }

  /**
   * Log an audit event
   */
  async logEvent(event: Omit<AuditEvent, 'eventId' | 'timestamp'>): Promise<string> {
    if (!this.config.enabled) {
      return '';
    }

    const auditEvent: AuditEvent = {
      ...event,
      eventId: uuidv4(),
      timestamp: new Date(),
    };

    // Apply sampling if configured
    if (this.config.sampling?.enabled) {
      if (Math.random() > this.config.sampling.rate) {
        return auditEvent.eventId;
      }
    }

    // Validate event
    this.validateEvent(auditEvent);

    // Add to queue
    this.eventQueue.push(auditEvent);

    // Check if batch should be flushed
    if (this.eventQueue.length >= this.config.batchSize) {
      await this.flushBatch();
    } else if (!this.batchTimeout) {
      this.setupBatchTimeout();
    }

    return auditEvent.eventId;
  }

  /**
   * Log an authentication event
   */
  async logAuthEvent(
    userId: string,
    success: boolean,
    sourceIp: string,
    userAgent?: string,
    metadata?: Record<string, any>
  ): Promise<string> {
    return this.logEvent({
      eventType: success ? AuditEventType.AUTH_LOGIN : AuditEventType.AUTH_FAILURE,
      actor: {
        userId,
      },
      action: {
        status: success ? AuditStatus.SUCCESS : AuditStatus.FAILURE,
        severity: success ? AuditSeverity.INFO : AuditSeverity.WARNING,
        description: success ? 'User authentication successful' : 'User authentication failed',
        errorMessage: success ? undefined : 'Invalid credentials',
      },
      context: {
        sourceIp,
        userAgent,
      },
      metadata,
    });
  }

  /**
   * Log a resource access event
   */
  async logResourceEvent(
    eventType: AuditEventType,
    userId: string,
    resourceId: string,
    resourceType: string,
    success: boolean,
    sourceIp: string,
    changes?: { before?: any; after?: any },
    metadata?: Record<string, any>
  ): Promise<string> {
    return this.logEvent({
      eventType,
      actor: {
        userId,
      },
      resource: {
        resourceId,
        resourceType,
      },
      action: {
        status: success ? AuditStatus.SUCCESS : AuditStatus.FAILURE,
        severity: success ? AuditSeverity.INFO : AuditSeverity.WARNING,
        description: `Resource ${eventType.split('.')[1]} operation`,
      },
      context: {
        sourceIp,
      },
      changes,
      metadata,
    });
  }

  /**
   * Log an authorization event
   */
  async logAuthzEvent(
    userId: string,
    action: 'grant' | 'revoke' | 'deny' | 'modify',
    role: string,
    resource?: string,
    sourceIp?: string,
    metadata?: Record<string, any>
  ): Promise<string> {
    const actionTypeMap = {
      grant: AuditEventType.AUTHZ_GRANT,
      revoke: AuditEventType.AUTHZ_REVOKE,
      deny: AuditEventType.AUTHZ_DENY,
      modify: AuditEventType.AUTHZ_MODIFY,
    };

    return this.logEvent({
      eventType: actionTypeMap[action],
      actor: {
        userId,
      },
      action: {
        status: AuditStatus.SUCCESS,
        severity: AuditSeverity.INFO,
        description: `Authorization ${action} - role: ${role}`,
      },
      context: {
        sourceIp: sourceIp || 'internal',
      },
      metadata: {
        role,
        resource,
        ...metadata,
      },
    });
  }

  /**
   * Log a security event
   */
  async logSecurityEvent(
    eventType: AuditEventType,
    description: string,
    severity: AuditSeverity,
    sourceIp: string,
    userId?: string,
    metadata?: Record<string, any>
  ): Promise<string> {
    return this.logEvent({
      eventType,
      actor: {
        userId: userId || 'system',
      },
      action: {
        status: AuditStatus.SUCCESS,
        severity,
        description,
      },
      context: {
        sourceIp,
      },
      metadata,
      compliance: {
        framework: 'SOC2',
      },
    });
  }

  /**
   * Query audit events
   */
  async queryEvents(filter: AuditEventFilter): Promise<AuditEventQueryResult> {
    return this.localStore.query(filter);
  }

  /**
   * Get audit statistics for a time period
   */
  async getStatistics(startDate: Date, endDate: Date) {
    return this.localStore.getStatistics(startDate, endDate);
  }

  /**
   * Flush pending events to all backends
   */
  async flushBatch(): Promise<void> {
    if (this.eventQueue.length === 0) {
      return;
    }

    const batch: AuditEventBatch = {
      batchId: uuidv4(),
      events: [...this.eventQueue],
      batchTimestamp: new Date(),
    };

    // Clear queue before processing to prevent duplicates
    this.eventQueue = [];

    // Clear batch timeout
    if (this.batchTimeout) {
      clearTimeout(this.batchTimeout);
      this.batchTimeout = null;
    }

    // Send to all backends in parallel
    await Promise.allSettled([
      this.localStore.saveBatch(batch),
      this.cloudLogging?.exportBatch(batch),
      this.siemExporter?.exportBatch(batch),
    ]);
  }

  /**
   * Setup batch timeout for periodic flushing
   */
  private setupBatchTimeout(): void {
    this.batchTimeout = setTimeout(async () => {
      await this.flushBatch();
    }, this.config.batchTimeoutMs);
  }

  /**
   * Validate audit event
   */
  private validateEvent(event: AuditEvent): void {
    if (!event.eventId) {
      throw new Error('Event ID is required');
    }
    if (!event.eventType) {
      throw new Error('Event type is required');
    }
    if (!event.actor?.userId) {
      throw new Error('Actor user ID is required');
    }
    if (!event.context?.sourceIp) {
      throw new Error('Context source IP is required');
    }
    if (!event.action) {
      throw new Error('Action information is required');
    }
  }

  /**
   * Initialize the audit system
   */
  async initialize(): Promise<void> {
    await this.localStore.initialize();
    if (this.cloudLogging) {
      await this.cloudLogging.initialize();
    }
    if (this.siemExporter) {
      await this.siemExporter.initialize();
    }
  }

  /**
   * Graceful shutdown
   */
  async shutdown(): Promise<void> {
    // Flush any pending events
    await this.flushBatch();

    if (this.batchTimeout) {
      clearTimeout(this.batchTimeout);
    }

    if (this.cloudLogging) {
      await this.cloudLogging.shutdown();
    }
    if (this.siemExporter) {
      await this.siemExporter.shutdown();
    }
    await this.localStore.shutdown();
  }

  /**
   * Get current queue size
   */
  getQueueSize(): number {
    return this.eventQueue.length;
  }

  /**
   * Force flush any pending events
   */
  async forceFlush(): Promise<void> {
    await this.flushBatch();
  }
}
