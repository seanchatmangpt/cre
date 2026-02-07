import { apiClient } from './api';
import { AuditLog, AuditFilters, PaginatedAuditLogs, AuditStats } from '../types/audit';

export const auditService = {
  async getAuditLogs(filters?: AuditFilters): Promise<PaginatedAuditLogs> {
    const response = await apiClient.get<PaginatedAuditLogs>('/audit-logs', filters);
    return response.data;
  },

  async getAuditLogById(id: string): Promise<AuditLog> {
    const response = await apiClient.get<AuditLog>(`/audit-logs/${id}`);
    return response.data;
  },

  async getAuditStats(): Promise<AuditStats> {
    const response = await apiClient.get<AuditStats>('/audit-logs/stats');
    return response.data;
  },

  async exportAuditLogs(filters?: AuditFilters): Promise<Blob> {
    const response = await apiClient.get<Blob>('/audit-logs/export', filters);
    return response.data;
  },
};
