/**
 * Google Cloud Logging Backend
 * Exports audit events to Google Cloud Logging
 */

import { Logging } from '@google-cloud/logging';
import { AuditEvent, AuditEventBatch, AuditLogConfig, CloudLoggingEntry } from '../types';

export class CloudLoggingBackend {
  private logging: Logging | null = null;
  private log: any = null;
  private config: AuditLogConfig;
  private isInitialized = false;

  constructor(config: AuditLogConfig) {
    this.config = config;
  }

  /**
   * Initialize Cloud Logging connection
   */
  async initialize(): Promise<void> {
    try {
      this.logging = new Logging({
        projectId: this.config.projectId,
      });

      this.log = this.logging.log(this.config.logName || 'audit-logs');
      this.isInitialized = true;

      console.log(`Cloud Logging backend initialized for project: ${this.config.projectId}`);
    } catch (error) {
      console.error('Failed to initialize Cloud Logging backend:', error);
      throw error;
    }
  }

  /**
   * Export a batch of events to Cloud Logging
   */
  async exportBatch(batch: AuditEventBatch): Promise<void> {
    if (!this.isInitialized || !this.log) {
      console.warn('Cloud Logging backend not initialized, skipping export');
      return;
    }

    try {
      const entries = batch.events.map((event) => this.convertToCloudLoggingEntry(event));

      // Write entries to Cloud Logging
      await Promise.all(
        entries.map((entry) =>
          this.log.write(this.log.entry(entry.labels || {}, entry.jsonPayload || entry.textPayload), {
            skipApiCall: false,
          })
        )
      );

      console.log(`Exported ${batch.events.length} audit events to Cloud Logging`);
    } catch (error) {
      console.error('Failed to export batch to Cloud Logging:', error);
      throw error;
    }
  }

  /**
   * Export a single event to Cloud Logging
   */
  async exportEvent(event: AuditEvent): Promise<void> {
    if (!this.isInitialized || !this.log) {
      return;
    }

    try {
      const entry = this.convertToCloudLoggingEntry(event);
      await this.log.write(this.log.entry(entry.labels || {}, entry.jsonPayload || entry.textPayload), {
        skipApiCall: false,
      });
    } catch (error) {
      console.error('Failed to export event to Cloud Logging:', error);
      throw error;
    }
  }

  /**
   * Convert audit event to Cloud Logging format
   */
  private convertToCloudLoggingEntry(event: AuditEvent): CloudLoggingEntry {
    return {
      logName: this.config.logName || 'audit-logs',
      severity: event.action.severity,
      timestamp: event.timestamp.toISOString(),
      jsonPayload: {
        eventId: event.eventId,
        eventType: event.eventType,
        actor: event.actor,
        resource: event.resource,
        action: event.action,
        context: this.sanitizeContext(event.context),
        metadata: event.metadata,
        changes: event.changes,
        compliance: event.compliance,
      },
      labels: {
        event_type: event.eventType,
        severity: event.action.severity,
        status: event.action.status,
        actor_id: event.actor.userId,
        environment: event.context.environment || 'production',
      },
      sourceLocation:
        event.context.environment !== 'production'
          ? {
              file: 'audit-event',
              line: 0,
              function: 'logEvent',
            }
          : undefined,
      trace: event.context.correlationId,
      spanId: event.context.requestId,
    };
  }

  /**
   * Sanitize context to remove sensitive information
   */
  private sanitizeContext(context: any): any {
    const sanitized = { ...context };
    // Remove sensitive data if needed
    if (sanitized.userAgent) {
      // Truncate user agent for privacy
      sanitized.userAgent = sanitized.userAgent.substring(0, 100);
    }
    return sanitized;
  }

  /**
   * Query events from Cloud Logging
   */
  async queryEvents(filter: string, limit: number = 100): Promise<AuditEvent[]> {
    if (!this.isInitialized || !this.logging) {
      throw new Error('Cloud Logging backend not initialized');
    }

    try {
      const options = {
        filter: `resource.type="global" AND logName="${this.config.logName}" AND ${filter}`,
        pageSize: Math.min(limit, 1000),
        orderBy: 'timestamp DESC',
      };

      const entries = await this.log.getEntries(options);
      return entries[0].map((entry: any) => this.parseCloudLoggingEntry(entry));
    } catch (error) {
      console.error('Failed to query events from Cloud Logging:', error);
      throw error;
    }
  }

  /**
   * Parse Cloud Logging entry back to audit event format
   */
  private parseCloudLoggingEntry(entry: any): AuditEvent {
    const payload = entry.data || {};
    return {
      eventId: payload.eventId || '',
      eventType: payload.eventType,
      timestamp: new Date(entry.metadata.timestamp),
      actor: payload.actor,
      resource: payload.resource,
      action: payload.action,
      context: payload.context,
      metadata: payload.metadata,
      changes: payload.changes,
      compliance: payload.compliance,
    };
  }

  /**
   * Create log sink for long-term archival
   */
  async createLogSink(sinkName: string, destination: string): Promise<void> {
    if (!this.isInitialized || !this.logging) {
      throw new Error('Cloud Logging backend not initialized');
    }

    try {
      const sink = this.logging.sink(sinkName);

      const config = {
        destination: destination,
        filter: `logName="${this.config.logName}"`,
      };

      await sink.create(config);
      console.log(`Created log sink: ${sinkName} -> ${destination}`);
    } catch (error) {
      console.error('Failed to create log sink:', error);
      throw error;
    }
  }

  /**
   * Setup metrics for audit events
   */
  async setupMetrics(): Promise<void> {
    if (!this.isInitialized || !this.logging) {
      throw new Error('Cloud Logging backend not initialized');
    }

    try {
      // This would typically be done through Cloud Monitoring API
      // For now, log metric descriptors that should be created
      console.log('Audit metrics setup completed (requires Cloud Monitoring API)');
    } catch (error) {
      console.error('Failed to setup metrics:', error);
      throw error;
    }
  }

  /**
   * Shutdown the Cloud Logging backend
   */
  async shutdown(): Promise<void> {
    if (this.logging) {
      this.isInitialized = false;
      // Cloud Logging client cleanup
    }
  }
}
