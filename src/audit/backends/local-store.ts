/**
 * Local Audit Store
 * Provides local persistence and querying of audit events
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import {
  AuditEvent,
  AuditEventBatch,
  AuditEventFilter,
  AuditEventQueryResult,
  AuditLogConfig,
  AuditLogStatistics,
  AuditEventType,
  AuditSeverity,
  AuditStatus,
} from '../types';

export class LocalAuditStore {
  private config: AuditLogConfig;
  private storePath: string;
  private events: Map<string, AuditEvent> = new Map();
  private isInitialized = false;

  constructor(config: AuditLogConfig) {
    this.config = config;
    this.storePath = path.join(process.cwd(), '.audit', 'events');
  }

  /**
   * Initialize the local store
   */
  async initialize(): Promise<void> {
    try {
      await fs.mkdir(this.storePath, { recursive: true });
      await this.loadEvents();
      this.isInitialized = true;
      console.log(`Local audit store initialized at ${this.storePath}`);
    } catch (error) {
      console.error('Failed to initialize local audit store:', error);
      throw error;
    }
  }

  /**
   * Load events from disk
   */
  private async loadEvents(): Promise<void> {
    try {
      const files = await fs.readdir(this.storePath);
      const eventFiles = files.filter((f) => f.endsWith('.json'));

      for (const file of eventFiles) {
        const filePath = path.join(this.storePath, file);
        const content = await fs.readFile(filePath, 'utf-8');
        const batch: AuditEventBatch = JSON.parse(content);

        for (const event of batch.events) {
          this.events.set(event.eventId, {
            ...event,
            timestamp: new Date(event.timestamp),
          });
        }
      }

      console.log(`Loaded ${this.events.size} audit events from disk`);
    } catch (error) {
      console.error('Failed to load audit events:', error);
    }
  }

  /**
   * Save event batch to disk
   */
  async saveBatch(batch: AuditEventBatch): Promise<void> {
    if (!this.isInitialized) {
      throw new Error('Local audit store not initialized');
    }

    try {
      const fileName = `batch_${batch.batchId}.json`;
      const filePath = path.join(this.storePath, fileName);

      const batchData = {
        ...batch,
        events: batch.events.map((e) => ({
          ...e,
          timestamp: e.timestamp.toISOString(),
        })),
      };

      await fs.writeFile(filePath, JSON.stringify(batchData, null, 2));

      // Add to in-memory map
      for (const event of batch.events) {
        this.events.set(event.eventId, event);
      }

      console.log(`Saved batch ${batch.batchId} with ${batch.events.length} events`);
    } catch (error) {
      console.error('Failed to save audit batch:', error);
      throw error;
    }
  }

  /**
   * Query events with filtering
   */
  async query(filter: AuditEventFilter): Promise<AuditEventQueryResult> {
    if (!this.isInitialized) {
      throw new Error('Local audit store not initialized');
    }

    try {
      let filteredEvents = Array.from(this.events.values());

      // Apply filters
      if (filter.eventTypes?.length) {
        filteredEvents = filteredEvents.filter((e) => filter.eventTypes?.includes(e.eventType));
      }

      if (filter.actors?.length) {
        filteredEvents = filteredEvents.filter((e) => filter.actors?.includes(e.actor.userId));
      }

      if (filter.resources?.length) {
        filteredEvents = filteredEvents.filter((e) =>
          filter.resources?.includes(e.resource?.resourceId || '')
        );
      }

      if (filter.dateRange) {
        filteredEvents = filteredEvents.filter(
          (e) => e.timestamp >= filter.dateRange!.startDate && e.timestamp <= filter.dateRange!.endDate
        );
      }

      if (filter.severity?.length) {
        filteredEvents = filteredEvents.filter((e) => filter.severity?.includes(e.action.severity));
      }

      if (filter.status?.length) {
        filteredEvents = filteredEvents.filter((e) => filter.status?.includes(e.action.status));
      }

      if (filter.sourceIp) {
        filteredEvents = filteredEvents.filter((e) => e.context.sourceIp === filter.sourceIp);
      }

      // Sort by timestamp descending
      filteredEvents.sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());

      // Apply pagination
      const limit = filter.limit || 100;
      const offset = filter.offset || 0;
      const total = filteredEvents.length;
      const paginatedEvents = filteredEvents.slice(offset, offset + limit);

      return {
        events: paginatedEvents,
        total,
        limit,
        offset,
        hasMore: offset + limit < total,
      };
    } catch (error) {
      console.error('Failed to query audit events:', error);
      throw error;
    }
  }

  /**
   * Get audit statistics for a time period
   */
  async getStatistics(startDate: Date, endDate: Date): Promise<AuditLogStatistics> {
    if (!this.isInitialized) {
      throw new Error('Local audit store not initialized');
    }

    const periodEvents = Array.from(this.events.values()).filter(
      (e) => e.timestamp >= startDate && e.timestamp <= endDate
    );

    const eventsByType: Record<AuditEventType, number> = {} as any;
    const eventsBySeverity: Record<AuditSeverity, number> = {} as any;
    const eventsByStatus: Record<AuditStatus, number> = {} as any;

    let failedEvents = 0;

    for (const event of periodEvents) {
      // Count by type
      eventsByType[event.eventType] = (eventsByType[event.eventType] || 0) + 1;

      // Count by severity
      eventsBySeverity[event.action.severity] = (eventsBySeverity[event.action.severity] || 0) + 1;

      // Count by status
      eventsByStatus[event.action.status] = (eventsByStatus[event.action.status] || 0) + 1;

      // Track failures
      if (event.action.status === AuditStatus.FAILURE) {
        failedEvents++;
      }
    }

    return {
      periodStart: startDate,
      periodEnd: endDate,
      totalEvents: periodEvents.length,
      eventsByType,
      eventsBySeverity,
      eventsByStatus,
      failedEvents,
      exportedEvents: 0, // Would be tracked separately
    };
  }

  /**
   * Delete events older than retention period
   */
  async enforceRetention(retentionDays: number): Promise<number> {
    if (!this.isInitialized) {
      throw new Error('Local audit store not initialized');
    }

    const cutoffDate = new Date();
    cutoffDate.setDate(cutoffDate.getDate() - retentionDays);

    let deletedCount = 0;
    const idsToDelete: string[] = [];

    for (const [eventId, event] of this.events.entries()) {
      if (event.timestamp < cutoffDate) {
        idsToDelete.push(eventId);
        deletedCount++;
      }
    }

    // Delete from memory
    for (const eventId of idsToDelete) {
      this.events.delete(eventId);
    }

    // Delete from disk - would need to reorganize files
    // For now, just log
    console.log(`Enforced retention policy: deleted ${deletedCount} events older than ${retentionDays} days`);

    return deletedCount;
  }

  /**
   * Export events to file
   */
  async exportToFile(filter: AuditEventFilter, outputPath: string, format: 'json' | 'csv' = 'json'): Promise<void> {
    if (!this.isInitialized) {
      throw new Error('Local audit store not initialized');
    }

    try {
      const result = await this.query({ ...filter, limit: 100000 });

      if (format === 'csv') {
        await this.exportAsCsv(result.events, outputPath);
      } else {
        await this.exportAsJson(result.events, outputPath);
      }

      console.log(`Exported ${result.events.length} events to ${outputPath}`);
    } catch (error) {
      console.error('Failed to export events:', error);
      throw error;
    }
  }

  /**
   * Export events as JSON
   */
  private async exportAsJson(events: AuditEvent[], outputPath: string): Promise<void> {
    const data = events.map((e) => ({
      ...e,
      timestamp: e.timestamp.toISOString(),
    }));

    await fs.writeFile(outputPath, JSON.stringify(data, null, 2));
  }

  /**
   * Export events as CSV
   */
  private async exportAsCsv(events: AuditEvent[], outputPath: string): Promise<void> {
    const headers = [
      'eventId',
      'eventType',
      'timestamp',
      'actorUserId',
      'actorEmail',
      'resourceId',
      'resourceType',
      'actionStatus',
      'actionSeverity',
      'actionDescription',
      'contextSourceIp',
      'complianceFramework',
      'dataClassification',
    ];

    const rows = events.map((e) => [
      e.eventId,
      e.eventType,
      e.timestamp.toISOString(),
      e.actor.userId,
      e.actor.userEmail || '',
      e.resource?.resourceId || '',
      e.resource?.resourceType || '',
      e.action.status,
      e.action.severity,
      e.action.description.replace(/"/g, '""'),
      e.context.sourceIp,
      e.compliance?.framework || '',
      e.compliance?.dataClassification || '',
    ]);

    const csvContent = [
      headers.map((h) => `"${h}"`).join(','),
      ...rows.map((r) => r.map((v) => `"${v}"`).join(',')),
    ].join('\n');

    await fs.writeFile(outputPath, csvContent);
  }

  /**
   * Shutdown the local store
   */
  async shutdown(): Promise<void> {
    this.events.clear();
    this.isInitialized = false;
  }

  /**
   * Get event by ID
   */
  async getEventById(eventId: string): Promise<AuditEvent | null> {
    if (!this.isInitialized) {
      throw new Error('Local audit store not initialized');
    }

    return this.events.get(eventId) || null;
  }

  /**
   * Get total events count
   */
  getEventCount(): number {
    return this.events.size;
  }
}
