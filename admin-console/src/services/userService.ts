import { apiClient } from './api';
import {
  User,
  CreateUserInput,
  UpdateUserInput,
  UserFilters,
  PaginatedUsers,
} from '../types/user';

export const userService = {
  async getUsers(filters?: UserFilters): Promise<PaginatedUsers> {
    const response = await apiClient.get<PaginatedUsers>('/users', filters);
    return response.data;
  },

  async getUserById(id: string): Promise<User> {
    const response = await apiClient.get<User>(`/users/${id}`);
    return response.data;
  },

  async createUser(input: CreateUserInput): Promise<User> {
    const response = await apiClient.post<User>('/users', input);
    return response.data;
  },

  async updateUser(id: string, input: UpdateUserInput): Promise<User> {
    const response = await apiClient.patch<User>(`/users/${id}`, input);
    return response.data;
  },

  async deleteUser(id: string): Promise<void> {
    await apiClient.delete(`/users/${id}`);
  },
};
