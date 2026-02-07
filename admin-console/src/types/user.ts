export enum UserRole {
  ADMIN = 'admin',
  MODERATOR = 'moderator',
  USER = 'user',
  VIEWER = 'viewer',
}

export enum UserStatus {
  ACTIVE = 'active',
  INACTIVE = 'inactive',
  SUSPENDED = 'suspended',
}

export interface User {
  id: string;
  email: string;
  username: string;
  firstName: string;
  lastName: string;
  role: UserRole;
  status: UserStatus;
  createdAt: string;
  updatedAt: string;
  lastLoginAt?: string;
  avatar?: string;
}

export interface CreateUserInput {
  email: string;
  username: string;
  firstName: string;
  lastName: string;
  role: UserRole;
  password: string;
}

export interface UpdateUserInput {
  email?: string;
  username?: string;
  firstName?: string;
  lastName?: string;
  role?: UserRole;
  status?: UserStatus;
}

export interface UserFilters {
  search?: string;
  role?: UserRole;
  status?: UserStatus;
  page?: number;
  pageSize?: number;
  sortBy?: keyof User;
  sortOrder?: 'asc' | 'desc';
}

export interface PaginatedUsers {
  data: User[];
  total: number;
  page: number;
  pageSize: number;
  totalPages: number;
}
