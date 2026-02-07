/**
 * Core type definitions for CRE Backend 2030
 */

export interface RequestContext {
  requestId: string;
  userId?: string;
  sessionId?: string;
  timestamp: Date;
  traceId: string;
  spanId: string;
  correlationId: string;
}

export interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: ApiError;
  meta?: {
    timestamp: Date;
    version: string;
    requestId: string;
  };
}

export interface ApiError {
  code: string;
  message: string;
  details?: Record<string, unknown>;
  statusCode: number;
}

export interface PaginationParams {
  page: number;
  pageSize: number;
  sort?: string;
  order?: 'ASC' | 'DESC';
}

export interface PaginatedResponse<T> {
  items: T[];
  total: number;
  page: number;
  pageSize: number;
  hasMore: boolean;
  totalPages: number;
}

export interface ServiceConfig {
  environment: 'development' | 'staging' | 'production';
  port: number;
  host: string;
  debug: boolean;
  logLevel: 'error' | 'warn' | 'info' | 'debug' | 'trace';
}

export interface GraphQLConfig extends ServiceConfig {
  graphqlPath: string;
  introspection: boolean;
  playground: boolean;
  corsOrigin: string[];
}

export interface RESTConfig extends ServiceConfig {
  apiVersion: string;
  apiPath: string;
}

export interface WebSocketConfig extends ServiceConfig {
  wsPath: string;
  heartbeatInterval: number;
  reconnectDelay: number;
}

export interface gRPCConfig extends ServiceConfig {
  grpcPort: number;
  protoPath: string;
  reflectionEnabled: boolean;
}

export interface ServiceMeshConfig {
  enabled: boolean;
  type: 'istio' | 'linkerd' | 'consul' | 'envoy';
  serviceName: string;
  namespace: string;
  labels: Record<string, string>;
  annotations: Record<string, string>;
}

export interface TracingConfig {
  enabled: boolean;
  jaegerHost: string;
  jaegerPort: number;
  samplingRate: number;
  serviceName: string;
  logSpans: boolean;
  maxTagLength: number;
}

export interface CacheConfig {
  type: 'redis' | 'memory' | 'hybrid';
  redis?: {
    host: string;
    port: number;
    password?: string;
    db: number;
    ttl: number;
  };
  memory?: {
    maxSize: number;
    maxAge: number;
  };
}

export interface AuthConfig {
  jwt: {
    secret: string;
    expiresIn: string;
    refreshExpiresIn: string;
    algorithm: string;
  };
  oauth: {
    enabled: boolean;
    providers: string[];
  };
  mfa: {
    enabled: boolean;
    providers: string[];
  };
}

export interface Event<T = unknown> {
  id: string;
  type: string;
  timestamp: Date;
  source: string;
  data: T;
  metadata: Record<string, unknown>;
  traceId: string;
}

export interface HealthStatus {
  status: 'UP' | 'DOWN' | 'DEGRADED';
  timestamp: Date;
  checks: Record<string, HealthCheck>;
  version: string;
}

export interface HealthCheck {
  status: 'UP' | 'DOWN' | 'UNKNOWN';
  message?: string;
  details?: Record<string, unknown>;
}

export interface MetricPoint {
  timestamp: Date;
  value: number;
  labels: Record<string, string>;
}

export interface ServiceMetrics {
  requests: MetricPoint[];
  errors: MetricPoint[];
  latency: MetricPoint[];
  activeConnections: number;
  throughput: number;
}
