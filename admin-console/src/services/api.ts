import axios, { AxiosInstance, AxiosError } from 'axios';
import { APIResponse, APIError } from '../types/api';

class APIClient {
  private client: AxiosInstance;

  constructor(baseURL: string = '/api') {
    this.client = axios.create({
      baseURL,
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    this.setupInterceptors();
  }

  private setupInterceptors() {
    this.client.interceptors.request.use(
      (config) => {
        const token = localStorage.getItem('auth_token');
        if (token) {
          config.headers.Authorization = `Bearer ${token}`;
        }
        return config;
      },
      (error) => Promise.reject(error)
    );

    this.client.interceptors.response.use(
      (response) => response,
      (error: AxiosError<APIError>) => {
        if (error.response?.status === 401) {
          localStorage.removeItem('auth_token');
          window.location.href = '/login';
        }
        return Promise.reject(error);
      }
    );
  }

  async get<T>(url: string, params?: Record<string, unknown>): Promise<APIResponse<T>> {
    const response = await this.client.get<APIResponse<T>>(url, { params });
    return response.data;
  }

  async post<T>(url: string, data?: unknown): Promise<APIResponse<T>> {
    const response = await this.client.post<APIResponse<T>>(url, data);
    return response.data;
  }

  async put<T>(url: string, data?: unknown): Promise<APIResponse<T>> {
    const response = await this.client.put<APIResponse<T>>(url, data);
    return response.data;
  }

  async patch<T>(url: string, data?: unknown): Promise<APIResponse<T>> {
    const response = await this.client.patch<APIResponse<T>>(url, data);
    return response.data;
  }

  async delete<T>(url: string): Promise<APIResponse<T>> {
    const response = await this.client.delete<APIResponse<T>>(url);
    return response.data;
  }
}

export const apiClient = new APIClient(import.meta.env.VITE_API_URL || '/api');
