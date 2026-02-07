/**
 * SIEM Exporter Backend
 * Exports audit events to SIEM systems in CEF and LEEF formats
 */

import * as net from 'net';
import { AuditEvent, AuditEventBatch, AuditLogConfig, CEFEvent, LEEFEvent } from '../types';

export class SiemExporter {
  private config: AuditLogConfig;
  private socket: net.Socket | null = null;
  private isConnected = false;
  private eventBuffer: string[] = [];
  private reconnectAttempts = 0;
  private maxReconnectAttempts = 5;

  constructor(config: AuditLogConfig) {
    this.config = config;
  }

  /**
   * Initialize SIEM connection
   */
  async initialize(): Promise<void> {
    try {
      if (!this.config.siemEndpoint) {
        console.warn('SIEM endpoint not configured, SIEM export disabled');
        return;
      }

      const [host, port] = this.config.siemEndpoint.split(':');
      const portNum = parseInt(port, 10) || 514;

      this.connect(host, portNum);
    } catch (error) {
      console.error('Failed to initialize SIEM exporter:', error);
    }
  }

  /**
   * Connect to SIEM endpoint
   */
  private connect(host: string, port: number): void {
    try {
      this.socket = net.createConnection(port, host);

      this.socket.on('connect', () => {
        console.log(`Connected to SIEM endpoint at ${host}:${port}`);
        this.isConnected = true;
        this.reconnectAttempts = 0;

        // Flush any buffered events
        this.flushBuffer();
      });

      this.socket.on('error', (error) => {
        console.error('SIEM connection error:', error);
        this.isConnected = false;
        this.handleReconnect(host, port);
      });

      this.socket.on('close', () => {
        console.log('SIEM connection closed');
        this.isConnected = false;
        this.handleReconnect(host, port);
      });
    } catch (error) {
      console.error('Failed to connect to SIEM:', error);
      this.handleReconnect(host, port);
    }
  }

  /**
   * Handle reconnection logic
   */
  private handleReconnect(host: string, port: number): void {
    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.reconnectAttempts++;
      const delay = Math.min(1000 * Math.pow(2, this.reconnectAttempts - 1), 30000);
      console.log(`Reconnecting to SIEM in ${delay}ms (attempt ${this.reconnectAttempts})`);

      setTimeout(() => {
        this.connect(host, port);
      }, delay);
    } else {
      console.error('Max SIEM reconnection attempts reached');
    }
  }

  /**
   * Export batch to SIEM
   */
  async exportBatch(batch: AuditEventBatch): Promise<void> {
    for (const event of batch.events) {
      await this.exportEvent(event);
    }
  }

  /**
   * Export single event to SIEM
   */
  async exportEvent(event: AuditEvent): Promise<void> {
    try {
      const formattedEvent = this.formatEventForSiem(event);

      if (this.isConnected && this.socket) {
        this.socket.write(formattedEvent + '\n');
      } else {
        // Buffer event if not connected
        this.eventBuffer.push(formattedEvent);

        // Prevent buffer from growing too large
        if (this.eventBuffer.length > 1000) {
          this.eventBuffer.shift();
          console.warn('SIEM event buffer overflow, discarding oldest event');
        }
      }
    } catch (error) {
      console.error('Failed to export event to SIEM:', error);
    }
  }

  /**
   * Format event for SIEM (CEF or LEEF)
   */
  private formatEventForSiem(event: AuditEvent): string {
    if (this.config.siemFormat === 'LEEF') {
      return this.formatAsLeef(event);
    } else {
      return this.formatAsCef(event);
    }
  }

  /**
   * Format event as CEF (Common Event Format)
   */
  private formatAsCef(event: AuditEvent): string {
    const severity = this.mapSeverityToCef(event.action.severity);
    const cefEvent: CEFEvent = {
      cefVersion: '0',
      deviceVendor: 'CRE',
      deviceProduct: 'CRE-AuditLog',
      deviceVersion: '1.0',
      signatureId: this.getSignatureId(event.eventType),
      name: event.eventType,
      severity: severity,
      extensions: {
        eventId: event.eventId,
        eventType: event.eventType,
        eventTimestamp: event.timestamp.toISOString(),
        actorUserId: event.actor.userId,
        actorEmail: event.actor.userEmail || 'N/A',
        actorName: event.actor.userName || 'N/A',
        actionStatus: event.action.status,
        actionDescription: event.action.description,
        actionErrorMessage: event.action.errorMessage || 'N/A',
        contextSourceIp: event.context.sourceIp,
        contextUserAgent: event.context.userAgent || 'N/A',
        contextRequestId: event.context.requestId || 'N/A',
        contextCorrelationId: event.context.correlationId || 'N/A',
        resourceId: event.resource?.resourceId || 'N/A',
        resourceType: event.resource?.resourceType || 'N/A',
        resourceName: event.resource?.resourceName || 'N/A',
        dataClassification: event.compliance?.dataClassification || 'N/A',
        complianceFramework: event.compliance?.framework || 'N/A',
        piiInvolved: event.compliance?.piiInvolved ? 'true' : 'false',
        ...this.flattenObject(event.metadata || {}),
      },
    };

    return this.buildCefString(cefEvent);
  }

  /**
   * Format event as LEEF (Log Event Extended Format)
   */
  private formatAsLeef(event: AuditEvent): string {
    const leefEvent: LEEFEvent = {
      version: '2.0',
      vendor: 'CRE',
      product: 'CRE-AuditLog',
      version_field: '1.0',
      eventId: this.getSignatureId(event.eventType),
      eventUuid: event.eventId,
      eventType: event.eventType,
      eventTimestamp: event.timestamp.getTime(),
      eventSeverity: event.action.severity,
      eventStatus: event.action.status,
      eventDescription: event.action.description,
      eventErrorMessage: event.action.errorMessage || '',
      actorUserId: event.actor.userId,
      actorEmail: event.actor.userEmail || '',
      actorName: event.actor.userName || '',
      contextSourceIp: event.context.sourceIp,
      contextUserAgent: event.context.userAgent || '',
      contextRequestId: event.context.requestId || '',
      contextCorrelationId: event.context.correlationId || '',
      resourceId: event.resource?.resourceId || '',
      resourceType: event.resource?.resourceType || '',
      resourceName: event.resource?.resourceName || '',
      dataClassification: event.compliance?.dataClassification || '',
      complianceFramework: event.compliance?.framework || '',
      piiInvolved: event.compliance?.piiInvolved ? '1' : '0',
      ...this.flattenObject(event.metadata || {}),
    };

    return this.buildLeefString(leefEvent);
  }

  /**
   * Build CEF string from event
   */
  private buildCefString(event: CEFEvent): string {
    const extensionsString = Object.entries(event.extensions)
      .map(([key, value]) => `${key}=${this.escapeCefValue(String(value))}`)
      .join(' ');

    return `CEF:${event.cefVersion}|${event.deviceVendor}|${event.deviceProduct}|${event.deviceVersion}|${event.signatureId}|${event.name}|${event.severity}|${extensionsString}`;
  }

  /**
   * Build LEEF string from event
   */
  private buildLeefString(event: LEEFEvent): string {
    const header = `LEEF:${event.version}|${event.vendor}|${event.product}|${event.version_field}|${event.eventId}|`;
    const fieldsString = Object.entries(event)
      .filter(([key]) => !['version', 'vendor', 'product', 'version_field', 'eventId'].includes(key))
      .map(([key, value]) => `${key}=${this.escapeLeefValue(String(value))}`)
      .join('\t');

    return header + fieldsString;
  }

  /**
   * Escape CEF values
   */
  private escapeCefValue(value: string): string {
    return value
      .replace(/\\/g, '\\\\')
      .replace(/=/g, '\\=')
      .replace(/\n/g, '\\n')
      .replace(/\r/g, '\\r');
  }

  /**
   * Escape LEEF values
   */
  private escapeLeefValue(value: string): string {
    return value
      .replace(/\\/g, '\\\\')
      .replace(/\t/g, '\\t')
      .replace(/\n/g, '\\n')
      .replace(/\r/g, '\\r');
  }

  /**
   * Map severity levels to CEF severity
   */
  private mapSeverityToCef(severity: string): string {
    const severityMap: Record<string, string> = {
      INFO: '2',
      WARNING: '4',
      ERROR: '6',
      CRITICAL: '9',
    };
    return severityMap[severity] || '2';
  }

  /**
   * Get signature ID for event type
   */
  private getSignatureId(eventType: string): string {
    // Create consistent hash for event type
    const charCodes = eventType.split('').reduce((acc, char) => acc + char.charCodeAt(0), 0);
    return `EVT${charCodes.toString().padStart(6, '0')}`;
  }

  /**
   * Flatten nested objects for SIEM export
   */
  private flattenObject(obj: Record<string, any>, prefix = ''): Record<string, any> {
    const result: Record<string, any> = {};

    for (const key in obj) {
      if (Object.prototype.hasOwnProperty.call(obj, key)) {
        const value = obj[key];
        const newKey = prefix ? `${prefix}_${key}` : key;

        if (value !== null && typeof value === 'object' && !Array.isArray(value)) {
          Object.assign(result, this.flattenObject(value, newKey));
        } else if (Array.isArray(value)) {
          result[newKey] = JSON.stringify(value);
        } else {
          result[newKey] = value;
        }
      }
    }

    return result;
  }

  /**
   * Flush buffered events
   */
  private flushBuffer(): void {
    if (this.eventBuffer.length === 0 || !this.isConnected || !this.socket) {
      return;
    }

    while (this.eventBuffer.length > 0) {
      const event = this.eventBuffer.shift();
      if (event) {
        this.socket.write(event + '\n');
      }
    }

    console.log('Flushed buffered SIEM events');
  }

  /**
   * Shutdown the SIEM exporter
   */
  async shutdown(): Promise<void> {
    if (this.socket) {
      this.socket.destroy();
      this.isConnected = false;
    }
  }

  /**
   * Get current buffer size
   */
  getBufferSize(): number {
    return this.eventBuffer.length;
  }

  /**
   * Check connection status
   */
  isConnectedToSiem(): boolean {
    return this.isConnected;
  }
}
