import { apiClient } from './api';
import { SystemMetrics, ServiceStatus, Alert } from '../types/monitoring';

export const monitoringService = {
  async getSystemMetrics(): Promise<SystemMetrics> {
    const response = await apiClient.get<SystemMetrics>('/monitoring/metrics');
    return response.data;
  },

  async getServiceStatus(): Promise<ServiceStatus[]> {
    const response = await apiClient.get<ServiceStatus[]>('/monitoring/services');
    return response.data;
  },

  async getAlerts(): Promise<Alert[]> {
    const response = await apiClient.get<Alert[]>('/monitoring/alerts');
    return response.data;
  },

  async acknowledgeAlert(id: string): Promise<void> {
    await apiClient.post(`/monitoring/alerts/${id}/acknowledge`);
  },
};
