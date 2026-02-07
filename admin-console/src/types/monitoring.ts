export interface SystemMetrics {
  cpu: {
    usage: number;
    cores: number;
    history: MetricPoint[];
  };
  memory: {
    used: number;
    total: number;
    percentage: number;
    history: MetricPoint[];
  };
  disk: {
    used: number;
    total: number;
    percentage: number;
  };
  network: {
    incoming: number;
    outgoing: number;
    history: MetricPoint[];
  };
}

export interface MetricPoint {
  timestamp: string;
  value: number;
}

export interface ServiceStatus {
  name: string;
  status: 'healthy' | 'degraded' | 'down';
  uptime: number;
  lastCheck: string;
  responseTime?: number;
  errorRate?: number;
}

export interface APIMetrics {
  totalRequests: number;
  successRate: number;
  averageResponseTime: number;
  errorRate: number;
  requestsPerMinute: number;
  topEndpoints: {
    endpoint: string;
    requests: number;
    avgResponseTime: number;
  }[];
}

export interface UserActivityMetrics {
  activeUsers: number;
  newUsers: number;
  totalUsers: number;
  userGrowth: MetricPoint[];
  topActions: {
    action: string;
    count: number;
  }[];
}

export interface AlertConfig {
  id: string;
  metric: string;
  condition: 'gt' | 'lt' | 'eq';
  threshold: number;
  enabled: boolean;
}

export interface Alert {
  id: string;
  severity: 'info' | 'warning' | 'error' | 'critical';
  message: string;
  metric: string;
  value: number;
  timestamp: string;
  acknowledged: boolean;
}
