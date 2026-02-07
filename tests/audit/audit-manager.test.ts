/**
 * Audit Manager Tests
 * Comprehensive test suite for audit event management
 */

import { describe, it, expect, beforeEach, afterEach } from '@jest/globals';
import { AuditManager } from '../../src/audit/audit-manager';
import {
  AuditEventType,
  AuditSeverity,
  AuditStatus,
  AuditLogConfig,
} from '../../src/audit/types';

describe('AuditManager', () => {
  let auditManager: AuditManager;
  let config: AuditLogConfig;

  beforeEach(async () => {
    config = {
      enabled: true,
      projectId: 'test-project',
      logName: 'test-audit-logs',
      batchSize: 10,
      batchTimeoutMs: 1000,
      enableCloudLogging: false,
      enableSiemExport: false,
      siemFormat: 'CEF',
      retentionPolicies: [],
      accessControls: [],
      complianceExports: [],
    };

    auditManager = new AuditManager(config);
    await auditManager.initialize();
  });

  afterEach(async () => {
    await auditManager.shutdown();
  });

  describe('Event Logging', () => {
    it('should log authentication event successfully', async () => {
      const eventId = await auditManager.logAuthEvent(
        'user-123',
        true,
        '192.168.1.1',
        'Mozilla/5.0'
      );

      expect(eventId).toBeDefined();
      expect(eventId).toMatch(/^[0-9a-f-]{36}$/); // UUID format
    });

    it('should log failed authentication event', async () => {
      const eventId = await auditManager.logAuthEvent(
        'user-123',
        false,
        '192.168.1.1'
      );

      expect(eventId).toBeDefined();
    });

    it('should log resource access event', async () => {
      const eventId = await auditManager.logResourceEvent(
        AuditEventType.RESOURCE_READ,
        'user-123',
        'resource-456',
        'Document',
        true,
        '192.168.1.1'
      );

      expect(eventId).toBeDefined();
    });

    it('should log authorization event', async () => {
      const eventId = await auditManager.logAuthzEvent(
        'admin-user',
        'grant',
        'viewer',
        'resource-456',
        '192.168.1.1'
      );

      expect(eventId).toBeDefined();
    });

    it('should log security event', async () => {
      const eventId = await auditManager.logSecurityEvent(
        AuditEventType.SECURITY_VIOLATION,
        'Unusual access pattern detected',
        AuditSeverity.WARNING,
        '192.168.1.1',
        'system'
      );

      expect(eventId).toBeDefined();
    });
  });

  describe('Event Batching', () => {
    it('should batch events when queue reaches batch size', async () => {
      for (let i = 0; i < 10; i++) {
        await auditManager.logAuthEvent(
          `user-${i}`,
          true,
          '192.168.1.1'
        );
      }

      // Wait for batch to be processed
      await new Promise(resolve => setTimeout(resolve, 100));

      // Queue should be empty after batch
      expect(auditManager.getQueueSize()).toBe(0);
    });

    it('should flush pending events on demand', async () => {
      await auditManager.logAuthEvent('user-123', true, '192.168.1.1');

      expect(auditManager.getQueueSize()).toBeGreaterThan(0);

      await auditManager.forceFlush();

      expect(auditManager.getQueueSize()).toBe(0);
    });

    it('should flush events after timeout', async () => {
      await auditManager.logAuthEvent('user-123', true, '192.168.1.1');

      // Wait for batch timeout
      await new Promise(resolve => setTimeout(resolve, config.batchTimeoutMs + 100));

      expect(auditManager.getQueueSize()).toBe(0);
    });
  });

  describe('Event Querying', () => {
    beforeEach(async () => {
      // Log various events
      await auditManager.logAuthEvent('user-123', true, '192.168.1.1');
      await auditManager.logAuthEvent('user-124', false, '192.168.1.2');
      await auditManager.logResourceEvent(
        AuditEventType.RESOURCE_READ,
        'user-123',
        'resource-456',
        'Document',
        true,
        '192.168.1.1'
      );

      await auditManager.forceFlush();

      // Wait for events to be stored
      await new Promise(resolve => setTimeout(resolve, 100));
    });

    it('should query events with no filter', async () => {
      const result = await auditManager.queryEvents({});

      expect(result.events.length).toBeGreaterThan(0);
      expect(result.total).toBeGreaterThan(0);
    });

    it('should query events by type', async () => {
      const result = await auditManager.queryEvents({
        eventTypes: [AuditEventType.AUTH_LOGIN],
      });

      expect(result.events.length).toBeGreaterThan(0);
      expect(result.events.every(e => e.eventType === AuditEventType.AUTH_LOGIN)).toBe(true);
    });

    it('should query events by actor', async () => {
      const result = await auditManager.queryEvents({
        actors: ['user-123'],
      });

      expect(result.events.every(e => e.actor.userId === 'user-123')).toBe(true);
    });

    it('should support pagination', async () => {
      const result = await auditManager.queryEvents({
        limit: 1,
        offset: 0,
      });

      expect(result.events.length).toBeLessThanOrEqual(1);
      expect(result.limit).toBe(1);
      expect(result.offset).toBe(0);
    });
  });

  describe('Statistics', () => {
    beforeEach(async () => {
      // Log various events
      await auditManager.logAuthEvent('user-123', true, '192.168.1.1');
      await auditManager.logAuthEvent('user-124', false, '192.168.1.2');
      await auditManager.logResourceEvent(
        AuditEventType.RESOURCE_CREATE,
        'user-123',
        'resource-456',
        'Document',
        true,
        '192.168.1.1'
      );

      await auditManager.forceFlush();
      await new Promise(resolve => setTimeout(resolve, 100));
    });

    it('should generate statistics for time period', async () => {
      const startDate = new Date();
      startDate.setHours(0, 0, 0, 0);
      const endDate = new Date();
      endDate.setDate(endDate.getDate() + 1);

      const stats = await auditManager.getStatistics(startDate, endDate);

      expect(stats.totalEvents).toBeGreaterThan(0);
      expect(stats.eventsByType).toBeDefined();
      expect(stats.eventsBySeverity).toBeDefined();
      expect(stats.eventsByStatus).toBeDefined();
    });

    it('should track failed events', async () => {
      const startDate = new Date();
      startDate.setHours(0, 0, 0, 0);
      const endDate = new Date();
      endDate.setDate(endDate.getDate() + 1);

      const stats = await auditManager.getStatistics(startDate, endDate);

      expect(stats.failedEvents).toBeGreaterThanOrEqual(0);
    });
  });

  describe('Sampling', () => {
    it('should apply sampling when configured', async () => {
      const sampledConfig: AuditLogConfig = {
        ...config,
        sampling: {
          enabled: true,
          rate: 0.5, // 50% sampling
        },
      };

      const sampledManager = new AuditManager(sampledConfig);
      await sampledManager.initialize();

      // Log multiple events and expect some to be sampled out
      const eventIds: string[] = [];
      for (let i = 0; i < 20; i++) {
        const id = await sampledManager.logAuthEvent(`user-${i}`, true, '192.168.1.1');
        if (id) {
          eventIds.push(id);
        }
      }

      // With 50% sampling, we shouldn't get all 20
      expect(eventIds.length).toBeLessThan(20);
      expect(eventIds.length).toBeGreaterThan(0);

      await sampledManager.shutdown();
    });
  });

  describe('Error Handling', () => {
    it('should handle logging without initialization gracefully', async () => {
      const uninitializedManager = new AuditManager(config);

      // Should not throw, just return empty string
      const eventId = await uninitializedManager.logAuthEvent('user-123', true, '192.168.1.1');

      expect(eventId).toBe('');
    });

    it('should disable logging when disabled', async () => {
      const disabledConfig: AuditLogConfig = {
        ...config,
        enabled: false,
      };

      const disabledManager = new AuditManager(disabledConfig);
      const eventId = await disabledManager.logAuthEvent('user-123', true, '192.168.1.1');

      expect(eventId).toBe('');
    });
  });

  describe('Integration', () => {
    it('should handle rapid event logging', async () => {
      const eventIds: string[] = [];

      for (let i = 0; i < 50; i++) {
        const id = await auditManager.logAuthEvent(
          `user-${i % 10}`,
          Math.random() > 0.5,
          `192.168.1.${(i % 254) + 1}`
        );
        eventIds.push(id);
      }

      expect(eventIds.length).toBe(50);
      expect(eventIds.every(id => id.length > 0)).toBe(true);

      await auditManager.forceFlush();

      const result = await auditManager.queryEvents({ limit: 100 });
      expect(result.events.length).toBeGreaterThan(0);
    });
  });
});
