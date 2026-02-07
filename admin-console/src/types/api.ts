export interface APIError {
  message: string;
  code: string;
  details?: Record<string, unknown>;
}

export interface APIResponse<T> {
  data: T;
  success: boolean;
  message?: string;
}

export interface PaginationParams {
  page: number;
  pageSize: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}

export interface PaginatedResponse<T> {
  data: T[];
  total: number;
  page: number;
  pageSize: number;
  totalPages: number;
}
