/**
 * Compliance Export Manager
 * Handles compliance-driven exports with retention policies and frameworks
 */

import { Storage } from '@google-cloud/storage';
import { BigQuery } from '@google-cloud/bigquery';
import { v4 as uuidv4 } from 'uuid';
import { ComplianceExport, AuditEventFilter, AuditEventType, AuditSeverity, AuditStatus } from '../types';
import { LocalAuditStore } from '../backends/local-store';

export class ComplianceExportManager {
  private storage: Storage | null = null;
  private bigquery: BigQuery | null = null;
  private localStore: LocalAuditStore;
  private projectId: string;
  private activeExports: Map<string, boolean> = new Map();

  constructor(localStore: LocalAuditStore, projectId: string) {
    this.localStore = localStore;
    this.projectId = projectId;

    // Initialize GCP clients
    this.storage = new Storage({ projectId });
    this.bigquery = new BigQuery({ projectId });
  }

  /**
   * Execute a compliance export
   */
  async executeExport(config: ComplianceExport): Promise<string> {
    const exportId = uuidv4();
    this.activeExports.set(exportId, true);

    try {
      console.log(`Starting compliance export: ${config.name} (ID: ${exportId})`);

      // Build filter based on export configuration
      const filter = this.buildFilter(config);

      // Query events
      const result = await this.localStore.query({ ...filter, limit: 100000 });
      console.log(`Exporting ${result.events.length} events for framework: ${config.framework}`);

      // Apply transformations
      let events = result.events;
      if (config.transformations) {
        events = this.applyTransformations(events, config.transformations);
      }

      // Export to destination(s)
      if (config.destination.type === 'gcs' || config.destination.type === 'gcs_and_bigquery') {
        await this.exportToGCS(events, config, exportId);
      }

      if (config.destination.type === 'bigquery' || config.destination.type === 'gcs_and_bigquery') {
        await this.exportToBigQuery(events, config, exportId);
      }

      console.log(`Compliance export ${exportId} completed successfully`);
      return exportId;
    } catch (error) {
      console.error(`Compliance export ${exportId} failed:`, error);
      throw error;
    } finally {
      this.activeExports.delete(exportId);
    }
  }

  /**
   * Build filter from export configuration
   */
  private buildFilter(config: ComplianceExport): AuditEventFilter {
    return {
      eventTypes: config.filters.eventTypes,
      dateRange: config.filters.dateRange,
      severity: config.filters.severity,
      status: config.filters.status,
    };
  }

  /**
   * Apply transformations to events
   */
  private applyTransformations(
    events: any[],
    transformations: {
      redactPii?: boolean;
      anonymize?: boolean;
      aggregateByHour?: boolean;
    }
  ): any[] {
    let transformed = events;

    if (transformations.redactPii) {
      transformed = transformed.map((e) => this.redactPii(e));
    }

    if (transformations.anonymize) {
      transformed = transformed.map((e) => this.anonymizeEvent(e));
    }

    if (transformations.aggregateByHour) {
      transformed = this.aggregateByHour(transformed);
    }

    return transformed;
  }

  /**
   * Redact PII from events
   */
  private redactPii(event: any): any {
    const redacted = { ...event };

    // Redact email addresses
    if (redacted.actor?.userEmail) {
      redacted.actor.userEmail = this.redactEmail(redacted.actor.userEmail);
    }

    // Redact IP addresses (partial)
    if (redacted.context?.sourceIp) {
      redacted.context.sourceIp = this.redactIp(redacted.context.sourceIp);
    }

    // Redact user agent
    if (redacted.context?.userAgent) {
      redacted.context.userAgent = '[REDACTED]';
    }

    return redacted;
  }

  /**
   * Anonymize event
   */
  private anonymizeEvent(event: any): any {
    const anonymized = { ...event };
    anonymized.actor.userId = this.hashString(anonymized.actor.userId);
    if (anonymized.actor.userEmail) {
      anonymized.actor.userEmail = this.hashString(anonymized.actor.userEmail);
    }
    if (anonymized.resource?.resourceId) {
      anonymized.resource.resourceId = this.hashString(anonymized.resource.resourceId);
    }
    return anonymized;
  }

  /**
   * Aggregate events by hour
   */
  private aggregateByHour(events: any[]): any[] {
    const hourly: Map<string, any> = new Map();

    for (const event of events) {
      const hour = new Date(event.timestamp);
      hour.setMinutes(0, 0, 0);
      const key = hour.toISOString();

      if (!hourly.has(key)) {
        hourly.set(key, {
          timestamp: hour,
          hour: key,
          count: 0,
          eventTypes: new Set(),
          severities: new Set(),
          statuses: new Set(),
          actors: new Set(),
          resources: new Set(),
        });
      }

      const bucket = hourly.get(key);
      bucket.count++;
      bucket.eventTypes.add(event.eventType);
      bucket.severities.add(event.action.severity);
      bucket.statuses.add(event.action.status);
      bucket.actors.add(event.actor.userId);
      if (event.resource?.resourceId) {
        bucket.resources.add(event.resource.resourceId);
      }
    }

    // Convert sets to arrays
    return Array.from(hourly.values()).map((b) => ({
      timestamp: b.timestamp,
      hour: b.hour,
      eventCount: b.count,
      uniqueEventTypes: Array.from(b.eventTypes),
      severities: Array.from(b.severities),
      statuses: Array.from(b.statuses),
      uniqueActors: b.actors.size,
      uniqueResources: b.resources.size,
    }));
  }

  /**
   * Export to Google Cloud Storage
   */
  private async exportToGCS(events: any[], config: ComplianceExport, exportId: string): Promise<void> {
    if (!this.storage || !config.destination.bucket) {
      throw new Error('GCS not configured');
    }

    try {
      const bucket = this.storage.bucket(config.destination.bucket);
      const fileName = this.generateFileName(config, exportId);
      const file = bucket.file(fileName);

      let content: string;
      if (config.format === 'CSV') {
        content = this.eventsToCSV(events);
      } else if (config.format === 'XML') {
        content = this.eventsToXML(events);
      } else if (config.format === 'PARQUET') {
        // Would use parquet library
        content = JSON.stringify(events, null, 2);
      } else {
        content = JSON.stringify(events, null, 2);
      }

      await file.save(content, {
        metadata: {
          contentType: this.getContentType(config.format),
          metadata: {
            exportId,
            framework: config.framework,
            timestamp: new Date().toISOString(),
          },
        },
      });

      console.log(`Exported to GCS: gs://${config.destination.bucket}/${fileName}`);
    } catch (error) {
      console.error('Failed to export to GCS:', error);
      throw error;
    }
  }

  /**
   * Export to BigQuery
   */
  private async exportToBigQuery(events: any[], config: ComplianceExport, exportId: string): Promise<void> {
    if (!this.bigquery || !config.destination.dataset || !config.destination.table) {
      throw new Error('BigQuery not configured');
    }

    try {
      const dataset = this.bigquery.dataset(config.destination.dataset);
      const table = dataset.table(config.destination.table);

      // Ensure table exists and has correct schema
      await this.ensureBigQueryTable(dataset, config.destination.table);

      // Insert rows
      await table.insert(events, {
        skipInvalidRows: true,
        ignoreUnknownValues: true,
      });

      console.log(
        `Exported ${events.length} events to BigQuery: ${config.destination.dataset}.${config.destination.table}`
      );
    } catch (error) {
      console.error('Failed to export to BigQuery:', error);
      throw error;
    }
  }

  /**
   * Ensure BigQuery table exists with correct schema
   */
  private async ensureBigQueryTable(dataset: any, tableName: string): Promise<void> {
    const schema = [
      { name: 'eventId', type: 'STRING', mode: 'REQUIRED' },
      { name: 'eventType', type: 'STRING', mode: 'REQUIRED' },
      { name: 'timestamp', type: 'TIMESTAMP', mode: 'REQUIRED' },
      {
        name: 'actor',
        type: 'RECORD',
        mode: 'REQUIRED',
        fields: [
          { name: 'userId', type: 'STRING', mode: 'REQUIRED' },
          { name: 'userEmail', type: 'STRING', mode: 'NULLABLE' },
          { name: 'userName', type: 'STRING', mode: 'NULLABLE' },
          { name: 'serviceAccount', type: 'BOOLEAN', mode: 'NULLABLE' },
        ],
      },
      {
        name: 'resource',
        type: 'RECORD',
        mode: 'NULLABLE',
        fields: [
          { name: 'resourceId', type: 'STRING', mode: 'NULLABLE' },
          { name: 'resourceType', type: 'STRING', mode: 'NULLABLE' },
          { name: 'resourceName', type: 'STRING', mode: 'NULLABLE' },
        ],
      },
      {
        name: 'action',
        type: 'RECORD',
        mode: 'REQUIRED',
        fields: [
          { name: 'status', type: 'STRING', mode: 'REQUIRED' },
          { name: 'severity', type: 'STRING', mode: 'REQUIRED' },
          { name: 'description', type: 'STRING', mode: 'REQUIRED' },
          { name: 'errorMessage', type: 'STRING', mode: 'NULLABLE' },
        ],
      },
      {
        name: 'context',
        type: 'RECORD',
        mode: 'REQUIRED',
        fields: [
          { name: 'sourceIp', type: 'STRING', mode: 'REQUIRED' },
          { name: 'userAgent', type: 'STRING', mode: 'NULLABLE' },
          { name: 'requestId', type: 'STRING', mode: 'NULLABLE' },
          { name: 'correlationId', type: 'STRING', mode: 'NULLABLE' },
          { name: 'environment', type: 'STRING', mode: 'NULLABLE' },
        ],
      },
      {
        name: 'compliance',
        type: 'RECORD',
        mode: 'NULLABLE',
        fields: [
          { name: 'framework', type: 'STRING', mode: 'NULLABLE' },
          { name: 'controlId', type: 'STRING', mode: 'NULLABLE' },
          { name: 'dataClassification', type: 'STRING', mode: 'NULLABLE' },
          { name: 'piiInvolved', type: 'BOOLEAN', mode: 'NULLABLE' },
        ],
      },
    ];

    try {
      const table = dataset.table(tableName);
      await table.create({ schema });
      console.log(`Created BigQuery table: ${tableName}`);
    } catch (error: any) {
      if (error.code === 409) {
        // Table already exists
        console.log(`BigQuery table ${tableName} already exists`);
      } else {
        throw error;
      }
    }
  }

  /**
   * Convert events to CSV format
   */
  private eventsToCSV(events: any[]): string {
    const headers = [
      'eventId',
      'eventType',
      'timestamp',
      'actorUserId',
      'actionStatus',
      'actionSeverity',
      'contextSourceIp',
      'framework',
    ];

    const rows = events.map((e) => [
      e.eventId,
      e.eventType,
      e.timestamp,
      e.actor?.userId || '',
      e.action?.status || '',
      e.action?.severity || '',
      e.context?.sourceIp || '',
      e.compliance?.framework || '',
    ]);

    return [headers.map((h) => `"${h}"`).join(','), ...rows.map((r) => r.map((v) => `"${v}"`).join(','))].join('\n');
  }

  /**
   * Convert events to XML format
   */
  private eventsToXML(events: any[]): string {
    let xml = '<?xml version="1.0" encoding="UTF-8"?>\n<events>\n';

    for (const event of events) {
      xml += `  <event>\n`;
      xml += `    <eventId>${this.escapeXml(event.eventId)}</eventId>\n`;
      xml += `    <eventType>${this.escapeXml(event.eventType)}</eventType>\n`;
      xml += `    <timestamp>${this.escapeXml(event.timestamp)}</timestamp>\n`;
      xml += `    <actor>\n`;
      xml += `      <userId>${this.escapeXml(event.actor?.userId)}</userId>\n`;
      xml += `    </actor>\n`;
      xml += `    <action>\n`;
      xml += `      <status>${this.escapeXml(event.action?.status)}</status>\n`;
      xml += `      <severity>${this.escapeXml(event.action?.severity)}</severity>\n`;
      xml += `    </action>\n`;
      xml += `  </event>\n`;
    }

    xml += '</events>';
    return xml;
  }

  /**
   * Helper functions
   */
  private generateFileName(config: ComplianceExport, exportId: string): string {
    const timestamp = new Date().toISOString().split('T')[0];
    const ext = this.getFileExtension(config.format);
    return `compliance-exports/${config.framework}/${timestamp}-${exportId}.${ext}`;
  }

  private getFileExtension(format: string): string {
    const extensionMap: Record<string, string> = {
      JSON: 'json',
      CSV: 'csv',
      XML: 'xml',
      PARQUET: 'parquet',
    };
    return extensionMap[format] || 'json';
  }

  private getContentType(format: string): string {
    const contentTypeMap: Record<string, string> = {
      JSON: 'application/json',
      CSV: 'text/csv',
      XML: 'application/xml',
      PARQUET: 'application/octet-stream',
    };
    return contentTypeMap[format] || 'application/json';
  }

  private redactEmail(email: string): string {
    const [name] = email.split('@');
    return name.substring(0, 2) + '***@[REDACTED]';
  }

  private redactIp(ip: string): string {
    const parts = ip.split('.');
    if (parts.length === 4) {
      return `${parts[0]}.${parts[1]}.XXX.XXX`;
    }
    return '[REDACTED]';
  }

  private hashString(str: string): string {
    // Simple hash function - replace with proper hashing in production
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return `HASH_${Math.abs(hash).toString(16)}`;
  }

  private escapeXml(str: string): string {
    return String(str || '')
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&apos;');
  }
}
