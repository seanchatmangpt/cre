import { apiClient } from './api';
import { ConfigItem, ConfigUpdate } from '../types/config';

export const configService = {
  async getConfigs(): Promise<ConfigItem[]> {
    const response = await apiClient.get<ConfigItem[]>('/config');
    return response.data;
  },

  async updateConfig(update: ConfigUpdate): Promise<ConfigItem> {
    const response = await apiClient.put<ConfigItem>('/config', update);
    return response.data;
  },

  async updateConfigs(updates: ConfigUpdate[]): Promise<ConfigItem[]> {
    const response = await apiClient.put<ConfigItem[]>('/config/batch', { updates });
    return response.data;
  },

  async resetConfig(key: string): Promise<ConfigItem> {
    const response = await apiClient.post<ConfigItem>(`/config/${key}/reset`);
    return response.data;
  },
};
