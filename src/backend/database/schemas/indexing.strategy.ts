import { Pool } from 'pg';

export interface IndexDefinition {
  name: string;
  table: string;
  columns: string[];
  type: 'BTREE' | 'HASH' | 'GIST' | 'BRIN' | 'GIN';
  unique?: boolean;
  partial?: string; // WHERE clause for partial index
  concurrent?: boolean;
  storage?: 'plain' | 'external' | 'extended' | 'main';
}

export interface IndexPerformance {
  indexName: string;
  tableName: string;
  indexSize: number;
  tableSize: number;
  usageCount: number;
  lastUsed?: Date;
  efficiency: number; // 0-100
}

export class IndexingStrategyManager {
  private pool: Pool;

  constructor(pool: Pool) {
    this.pool = pool;
  }

  async createBTreeIndex(
    tableName: string,
    columns: string[],
    indexName?: string,
    unique: boolean = false,
    concurrent: boolean = true,
    schema: string = 'public'
  ): Promise<void> {
    const name = indexName || this.generateIndexName(tableName, columns, 'btree');
    const uniqueStr = unique ? 'UNIQUE' : '';
    const concurrentStr = concurrent ? 'CONCURRENTLY' : '';
    const columnsList = columns.join(',');

    const query = `
      CREATE ${uniqueStr} INDEX ${concurrentStr} IF NOT EXISTS ${name}
      ON ${schema}.${tableName} (${columnsList})
      USING BTREE;
    `;

    await this.pool.query(query);
    console.log(`B-tree index ${name} created on ${schema}.${tableName}(${columnsList})`);
  }

  async createHashIndex(
    tableName: string,
    column: string,
    indexName?: string,
    schema: string = 'public'
  ): Promise<void> {
    const name = indexName || this.generateIndexName(tableName, [column], 'hash');

    const query = `
      CREATE INDEX IF NOT EXISTS ${name}
      ON ${schema}.${tableName} (${column})
      USING HASH;
    `;

    await this.pool.query(query);
    console.log(`Hash index ${name} created on ${schema}.${tableName}(${column})`);
  }

  async createGistIndex(
    tableName: string,
    column: string,
    indexName?: string,
    schema: string = 'public'
  ): Promise<void> {
    const name = indexName || this.generateIndexName(tableName, [column], 'gist');

    const query = `
      CREATE INDEX IF NOT EXISTS ${name}
      ON ${schema}.${tableName} USING GIST (${column});
    `;

    await this.pool.query(query);
    console.log(`GiST index ${name} created on ${schema}.${tableName}(${column})`);
  }

  async createBrinIndex(
    tableName: string,
    column: string,
    pagesPerRange: number = 128,
    indexName?: string,
    schema: string = 'public'
  ): Promise<void> {
    const name = indexName || this.generateIndexName(tableName, [column], 'brin');

    const query = `
      CREATE INDEX IF NOT EXISTS ${name}
      ON ${schema}.${tableName} USING BRIN (${column})
      WITH (pages_per_range = ${pagesPerRange});
    `;

    await this.pool.query(query);
    console.log(`BRIN index ${name} created on ${schema}.${tableName}(${column})`);
  }

  async createPartialIndex(
    tableName: string,
    columns: string[],
    whereClause: string,
    indexName?: string,
    schema: string = 'public'
  ): Promise<void> {
    const name = indexName || this.generateIndexName(tableName, columns, 'partial');
    const columnsList = columns.join(',');

    const query = `
      CREATE INDEX IF NOT EXISTS ${name}
      ON ${schema}.${tableName} (${columnsList})
      WHERE ${whereClause};
    `;

    await this.pool.query(query);
    console.log(`Partial index ${name} created on ${schema}.${tableName} WHERE ${whereClause}`);
  }

  async createGinIndex(
    tableName: string,
    column: string,
    indexName?: string,
    schema: string = 'public'
  ): Promise<void> {
    const name = indexName || this.generateIndexName(tableName, [column], 'gin');

    const query = `
      CREATE INDEX IF NOT EXISTS ${name}
      ON ${schema}.${tableName} USING GIN (${column});
    `;

    await this.pool.query(query);
    console.log(`GIN index ${name} created on ${schema}.${tableName}(${column})`);
  }

  async createCompositeIndex(
    tableName: string,
    columns: string[],
    indexName?: string,
    schema: string = 'public'
  ): Promise<void> {
    const name = indexName || this.generateIndexName(tableName, columns, 'composite');
    const columnsList = columns.join(',');

    const query = `
      CREATE INDEX IF NOT EXISTS ${name}
      ON ${schema}.${tableName} (${columnsList});
    `;

    await this.pool.query(query);
    console.log(`Composite index ${name} created on ${schema}.${tableName}(${columnsList})`);
  }

  async createExpressionIndex(
    tableName: string,
    expression: string,
    indexName?: string,
    schema: string = 'public'
  ): Promise<void> {
    const name = indexName || this.generateIndexName(tableName, ['expr'], 'expr');

    const query = `
      CREATE INDEX IF NOT EXISTS ${name}
      ON ${schema}.${tableName} (${expression});
    `;

    await this.pool.query(query);
    console.log(`Expression index ${name} created on ${schema}.${tableName}(${expression})`);
  }

  async dropIndex(indexName: string, schema: string = 'public', concurrent: boolean = true): Promise<void> {
    const concurrentStr = concurrent ? 'CONCURRENTLY' : '';

    const query = `DROP INDEX IF EXISTS ${concurrentStr} ${schema}.${indexName}`;

    await this.pool.query(query);
    console.log(`Index ${indexName} dropped`);
  }

  async reindexTable(tableName: string, schema: string = 'public', concurrent: boolean = true): Promise<void> {
    const concurrentStr = concurrent ? 'CONCURRENTLY' : '';

    const query = `REINDEX TABLE ${concurrentStr} ${schema}.${tableName}`;

    await this.pool.query(query);
    console.log(`Table ${schema}.${tableName} reindexed`);
  }

  async analyzeIndex(indexName: string, schema: string = 'public'): Promise<any> {
    const result = await this.pool.query(
      `SELECT
        schemaname,
        tablename,
        indexname,
        indexdef
       FROM pg_indexes
       WHERE indexname = $1 AND schemaname = $2`,
      [indexName, schema]
    );

    if (result.rows.length === 0) {
      throw new Error(`Index ${indexName} not found`);
    }

    return result.rows[0];
  }

  async getIndexPerformance(tableName: string, schema: string = 'public'): Promise<IndexPerformance[]> {
    const result = await this.pool.query(
      `SELECT
        t.relname as table_name,
        i.relname as index_name,
        ROUND(pg_relation_size(i.oid) / 1024.0 / 1024.0, 2) as index_size_mb,
        ROUND(pg_total_relation_size(t.oid) / 1024.0 / 1024.0, 2) as table_size_mb,
        COALESCE(idx_scan, 0) as usage_count,
        COALESCE(idx_tup_read, 0) as tuples_read,
        COALESCE(idx_tup_fetch, 0) as tuples_fetched,
        last_idx_scan as last_used,
        ROUND(
          CASE
            WHEN idx_scan = 0 THEN 0
            ELSE (idx_tup_fetch::float / NULLIF(idx_tup_read, 0)) * 100
          END, 2
        ) as efficiency_percent
       FROM pg_class t
       JOIN pg_index ix ON t.oid = ix.indrelid
       JOIN pg_class i ON i.oid = ix.indexrelid
       JOIN pg_stat_user_indexes si ON si.indexrelname = i.relname
       WHERE t.relname = $1
       AND t.relnamespace = (SELECT oid FROM pg_namespace WHERE nspname = $2)
       ORDER BY idx_scan DESC`,
      [tableName, schema]
    );

    return result.rows.map(row => ({
      indexName: row.index_name,
      tableName: row.table_name,
      indexSize: row.index_size_mb,
      tableSize: row.table_size_mb,
      usageCount: row.usage_count,
      lastUsed: row.last_used,
      efficiency: row.efficiency_percent,
    }));
  }

  async identifyUnusedIndexes(schema: string = 'public'): Promise<any[]> {
    const result = await this.pool.query(
      `SELECT
        t.relname as table_name,
        i.relname as index_name,
        ROUND(pg_relation_size(i.oid) / 1024.0 / 1024.0, 2) as size_mb,
        COALESCE(idx_scan, 0) as scan_count,
        last_idx_scan
       FROM pg_class t
       JOIN pg_index ix ON t.oid = ix.indrelid
       JOIN pg_class i ON i.oid = ix.indexrelid
       LEFT JOIN pg_stat_user_indexes si ON si.indexrelid = i.oid
       WHERE t.relnamespace = (SELECT oid FROM pg_namespace WHERE nspname = $1)
       AND idx_scan = 0
       AND i.relname NOT LIKE 'pg_toast%'
       AND ix.indisprimary = FALSE
       AND ix.indisunique = FALSE
       ORDER BY pg_relation_size(i.oid) DESC`,
      [schema]
    );

    return result.rows;
  }

  async identifyMissingIndexes(schema: string = 'public'): Promise<any[]> {
    const result = await this.pool.query(
      `SELECT
        schemaname,
        tablename,
        attname,
        n_distinct,
        correlation
       FROM pg_stats
       WHERE schemaname = $1
       AND n_distinct > 100
       AND ABS(correlation) < 0.1
       ORDER BY n_distinct DESC
       LIMIT 20`,
      [schema]
    );

    return result.rows;
  }

  async suggestIndexStrategy(tableName: string, schema: string = 'public'): Promise<IndexDefinition[]> {
    // Analyze table structure and usage patterns to suggest indexes
    const result = await this.pool.query(
      `SELECT
        a.attname,
        t.typname,
        n_distinct,
        null_frac,
        avg_width,
        correlation
       FROM pg_stats
       JOIN pg_attribute a ON a.attname = pg_stats.attname
       JOIN pg_type t ON a.atttypid = t.oid
       WHERE tablename = $1
       AND schemaname = $2
       AND NOT (attnum <= 0)
       ORDER BY n_distinct DESC`,
      [tableName, schema]
    );

    const suggestions: IndexDefinition[] = [];

    for (const row of result.rows) {
      if (row.n_distinct > 100) {
        suggestions.push({
          name: `idx_${tableName}_${row.attname}`,
          table: tableName,
          columns: [row.attname],
          type: 'BTREE',
          concurrent: true,
        });
      }

      if (row.typname === 'jsonb' || row.typname === 'json') {
        suggestions.push({
          name: `idx_${tableName}_${row.attname}_gin`,
          table: tableName,
          columns: [row.attname],
          type: 'GIN',
          concurrent: true,
        });
      }
    }

    return suggestions;
  }

  private generateIndexName(tableName: string, columns: string[], indexType: string): string {
    const prefix = indexType.toLowerCase();
    const suffix = columns.join('_').toLowerCase();
    return `idx_${tableName}_${suffix}_${prefix}`;
  }

  async getIndexFragmentation(indexName: string, schema: string = 'public'): Promise<number> {
    const result = await this.pool.query(
      `SELECT
        ROUND(100 * (pg_relation_size(i.oid) - pg_relation_size(i.oid, 'main')) /
          NULLIF(pg_relation_size(i.oid), 0), 2) as fragmentation_percent
       FROM pg_class i
       WHERE i.relname = $1
       AND i.relnamespace = (SELECT oid FROM pg_namespace WHERE nspname = $2)`,
      [indexName, schema]
    );

    return result.rows[0]?.fragmentation_percent || 0;
  }

  async optimizeIndexes(schema: string = 'public', fragmentationThreshold: number = 20): Promise<string[]> {
    const optimized: string[] = [];

    // Get fragmented indexes
    const result = await this.pool.query(
      `SELECT
        i.relname
       FROM pg_class t
       JOIN pg_index ix ON t.oid = ix.indrelid
       JOIN pg_class i ON i.oid = ix.indexrelid
       WHERE t.relnamespace = (SELECT oid FROM pg_namespace WHERE nspname = $1)
       AND (pg_relation_size(i.oid) - pg_relation_size(i.oid, 'main')) /
           NULLIF(pg_relation_size(i.oid), 0) > $2`,
      [schema, fragmentationThreshold / 100]
    );

    for (const row of result.rows) {
      try {
        await this.dropIndex(row.relname, schema);
        optimized.push(row.relname);
      } catch (error) {
        console.error(`Failed to optimize index ${row.relname}: ${error}`);
      }
    }

    return optimized;
  }
}
