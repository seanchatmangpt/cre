import { Pool, PoolConfig } from 'pg';
import Redis from 'ioredis';

export interface DatabaseConfig {
  primary: PostgresConfig;
  replicas: PostgresConfig[];
  redis: RedisConfig;
  migration: MigrationConfig;
  multiTenant: MultiTenantConfig;
  performance: PerformanceConfig;
}

export interface PostgresConfig extends PoolConfig {
  host: string;
  port: number;
  database: string;
  user: string;
  password: string;
  ssl?: boolean | string;
  maxConnections?: number;
  idleTimeout?: number;
  replica?: boolean;
}

export interface RedisConfig {
  host: string;
  port: number;
  password?: string;
  db?: number;
  clusters?: Array<{ host: string; port: number }>;
  enableOfflineQueue?: boolean;
  maxRetriesPerRequest?: number;
}

export interface MultiTenantConfig {
  enabled: boolean;
  strategy: 'schema' | 'row-level' | 'hybrid';
  isolationLevel: 'READ_UNCOMMITTED' | 'READ_COMMITTED' | 'REPEATABLE_READ' | 'SERIALIZABLE';
  enableRowLevelSecurity: boolean;
}

export interface MigrationConfig {
  directory: string;
  table: string;
  timestampFormat: 'unix' | 'iso8601';
  dryRun?: boolean;
  verbose?: boolean;
}

export interface PerformanceConfig {
  enableQueryCaching: boolean;
  cacheExpiry: number; // milliseconds
  enableConnectionPooling: boolean;
  poolSize: number;
  statementTimeout: number; // milliseconds
  queryTimeout: number; // milliseconds
  enableParallelQueries: boolean;
  maxParallelQueries: number;
}

export interface ShardConfig {
  enabled: boolean;
  shardKey: string; // Column name to shard on
  numberOfShards: number;
  algorithm: 'consistent-hash' | 'range' | 'modulo';
  replicationFactor: number;
}

export interface IndexingStrategyConfig {
  enableBTreeIndexes: boolean;
  enableHashIndexes: boolean;
  enableGistIndexes: boolean;
  enableBrinIndexes: boolean;
  enablePartialIndexes: boolean;
  enableConcurrentIndexing: boolean;
  indexMaintenanceWindow?: {
    day: number; // 0-6
    hour: number; // 0-23
    minute: number;
  };
}

export interface TemporalConfig {
  enabled: boolean;
  systemTimeColumns: {
    startTime: string;
    endTime: string;
  };
  enableTimeTravel: boolean;
  enableVersioning: boolean;
  retentionPolicy?: {
    archiveAfterDays: number;
    deleteAfterDays?: number;
  };
}

export class DatabaseConfigManager {
  private config: DatabaseConfig;

  constructor(config: DatabaseConfig) {
    this.config = this.validateAndNormalizeConfig(config);
  }

  private validateAndNormalizeConfig(config: DatabaseConfig): DatabaseConfig {
    if (!config.primary) {
      throw new Error('Primary database configuration is required');
    }

    if (!config.redis) {
      throw new Error('Redis configuration is required');
    }

    config.primary.maxConnections = config.primary.maxConnections || 20;
    config.primary.idleTimeout = config.primary.idleTimeout || 30000;

    if (config.replicas && config.replicas.length > 0) {
      config.replicas.forEach((replica) => {
        replica.replica = true;
        replica.maxConnections = replica.maxConnections || 10;
      });
    }

    return config;
  }

  getConfig(): DatabaseConfig {
    return this.config;
  }

  getPrimaryConfig(): PostgresConfig {
    return this.config.primary;
  }

  getReplicaConfigs(): PostgresConfig[] {
    return this.config.replicas || [];
  }

  getRedisConfig(): RedisConfig {
    return this.config.redis;
  }

  getRandomReplica(): PostgresConfig | undefined {
    const replicas = this.getReplicaConfigs();
    if (replicas.length === 0) return undefined;
    return replicas[Math.floor(Math.random() * replicas.length)];
  }

  isMultiTenantEnabled(): boolean {
    return this.config.multiTenant?.enabled || false;
  }

  getMultiTenantStrategy(): 'schema' | 'row-level' | 'hybrid' {
    return this.config.multiTenant?.strategy || 'schema';
  }
}

export const createDefaultConfig = (): DatabaseConfig => ({
  primary: {
    host: process.env.DB_HOST || 'localhost',
    port: parseInt(process.env.DB_PORT || '5432'),
    database: process.env.DB_NAME || 'app_db',
    user: process.env.DB_USER || 'postgres',
    password: process.env.DB_PASSWORD || 'postgres',
    maxConnections: 20,
    idleTimeout: 30000,
  },
  replicas: process.env.DB_REPLICAS
    ? JSON.parse(process.env.DB_REPLICAS)
    : [],
  redis: {
    host: process.env.REDIS_HOST || 'localhost',
    port: parseInt(process.env.REDIS_PORT || '6379'),
    password: process.env.REDIS_PASSWORD,
    db: 0,
  },
  migration: {
    directory: '/home/user/cre/src/backend/database/migrations',
    table: 'schema_migrations',
    timestampFormat: 'unix',
  },
  multiTenant: {
    enabled: process.env.MULTI_TENANT_ENABLED === 'true',
    strategy: 'schema',
    isolationLevel: 'READ_COMMITTED',
    enableRowLevelSecurity: true,
  },
  performance: {
    enableQueryCaching: true,
    cacheExpiry: 300000, // 5 minutes
    enableConnectionPooling: true,
    poolSize: 20,
    statementTimeout: 30000,
    queryTimeout: 60000,
    enableParallelQueries: true,
    maxParallelQueries: 4,
  },
});
