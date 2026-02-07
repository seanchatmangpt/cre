export enum AuditAction {
  CREATE = 'create',
  READ = 'read',
  UPDATE = 'update',
  DELETE = 'delete',
  LOGIN = 'login',
  LOGOUT = 'logout',
  PERMISSION_CHANGE = 'permission_change',
  CONFIG_CHANGE = 'config_change',
  EXPORT = 'export',
  IMPORT = 'import',
}

export enum AuditSeverity {
  INFO = 'info',
  WARNING = 'warning',
  ERROR = 'error',
  CRITICAL = 'critical',
}

export interface AuditLog {
  id: string;
  timestamp: string;
  userId: string;
  username: string;
  action: AuditAction;
  resource: string;
  resourceId?: string;
  severity: AuditSeverity;
  ipAddress: string;
  userAgent: string;
  details?: Record<string, unknown>;
  status: 'success' | 'failure';
  changes?: {
    before?: Record<string, unknown>;
    after?: Record<string, unknown>;
  };
}

export interface AuditFilters {
  search?: string;
  userId?: string;
  action?: AuditAction;
  severity?: AuditSeverity;
  resource?: string;
  status?: 'success' | 'failure';
  startDate?: string;
  endDate?: string;
  page?: number;
  pageSize?: number;
  sortBy?: keyof AuditLog;
  sortOrder?: 'asc' | 'desc';
}

export interface PaginatedAuditLogs {
  data: AuditLog[];
  total: number;
  page: number;
  pageSize: number;
  totalPages: number;
}

export interface AuditStats {
  totalLogs: number;
  successRate: number;
  topActions: {
    action: AuditAction;
    count: number;
  }[];
  topUsers: {
    userId: string;
    username: string;
    actionCount: number;
  }[];
  activityByHour: {
    hour: number;
    count: number;
  }[];
}
