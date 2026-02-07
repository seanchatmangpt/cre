import { Pool } from 'pg';

export interface TemporalRecord {
  id: string;
  data: any;
  validFromTime: Date;
  validToTime: Date;
  transactionTime: Date;
  version: number;
  operation: 'INSERT' | 'UPDATE' | 'DELETE';
}

export interface TimeSlice {
  timestamp: Date;
  records: TemporalRecord[];
}

export class TemporalTableManager {
  private pool: Pool;

  constructor(pool: Pool) {
    this.pool = pool;
  }

  async createTemporalTable(
    tableName: string,
    columns: Record<string, string>,
    schema: string = 'public'
  ): Promise<void> {
    const client = await this.pool.connect();
    try {
      await client.query('BEGIN');

      const fullTableName = `${schema}.${tableName}`;
      const historyTableName = `${fullTableName}_history`;

      // Build column definitions with temporal columns
      const columnDefs = Object.entries(columns)
        .map(([name, type]) => `${name} ${type}`)
        .join(',\n    ');

      // Create main table
      await client.query(`
        CREATE TABLE IF NOT EXISTS ${fullTableName} (
          id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
          ${columnDefs},
          valid_from TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
          valid_to TIMESTAMP NOT NULL DEFAULT '9999-12-31 23:59:59',
          transaction_time TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
          version BIGINT NOT NULL DEFAULT 1,
          created_by UUID,
          updated_by UUID,
          CONSTRAINT valid_time_range CHECK (valid_from < valid_to)
        );
      `);

      // Create history table (unlogged for performance)
      await client.query(`
        CREATE UNLOGGED TABLE IF NOT EXISTS ${historyTableName} (
          id UUID NOT NULL,
          ${columnDefs},
          valid_from TIMESTAMP NOT NULL,
          valid_to TIMESTAMP NOT NULL,
          transaction_time TIMESTAMP NOT NULL,
          version BIGINT NOT NULL,
          operation VARCHAR(10) NOT NULL,
          created_by UUID,
          updated_by UUID,
          archived_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
      `);

      // Create indexes for temporal queries
      await client.query(`
        CREATE INDEX IF NOT EXISTS idx_${tableName}_valid_time
        ON ${fullTableName}(valid_from, valid_to);
      `);

      await client.query(`
        CREATE INDEX IF NOT EXISTS idx_${tableName}_transaction_time
        ON ${fullTableName}(transaction_time DESC);
      `);

      await client.query(`
        CREATE INDEX IF NOT EXISTS idx_${historyTableName}_id_transaction_time
        ON ${historyTableName}(id, transaction_time DESC);
      `);

      // Create trigger function for temporal tracking
      const triggerFunctionName = `${tableName}_temporal_trigger`;

      await client.query(`
        CREATE OR REPLACE FUNCTION ${schema}.${triggerFunctionName}()
        RETURNS TRIGGER AS $$
        BEGIN
          IF TG_OP = 'INSERT' THEN
            NEW.transaction_time := CURRENT_TIMESTAMP;
            NEW.version := 1;
            RETURN NEW;
          END IF;

          IF TG_OP = 'UPDATE' THEN
            -- Close the old record
            UPDATE ${fullTableName}
            SET valid_to = CURRENT_TIMESTAMP
            WHERE id = NEW.id AND valid_to = '9999-12-31 23:59:59';

            -- Archive old version
            INSERT INTO ${historyTableName}
            SELECT OLD.*, TG_OP, CURRENT_TIMESTAMP FROM ${fullTableName}
            WHERE id = OLD.id LIMIT 1;

            NEW.transaction_time := CURRENT_TIMESTAMP;
            NEW.valid_from := CURRENT_TIMESTAMP;
            NEW.valid_to := '9999-12-31 23:59:59';
            NEW.version := OLD.version + 1;
            NEW.updated_by := COALESCE(NEW.updated_by, OLD.updated_by);

            RETURN NEW;
          END IF;

          IF TG_OP = 'DELETE' THEN
            -- Soft delete - mark as deleted
            UPDATE ${fullTableName}
            SET valid_to = CURRENT_TIMESTAMP
            WHERE id = OLD.id;

            -- Archive deletion
            INSERT INTO ${historyTableName}
            SELECT OLD.*, 'DELETE', CURRENT_TIMESTAMP;

            RETURN NULL;
          END IF;

          RETURN NEW;
        END;
        $$ LANGUAGE plpgsql;
      `);

      // Create trigger
      await client.query(`
        DROP TRIGGER IF EXISTS ${tableName}_temporal_trigger ON ${fullTableName};
        CREATE TRIGGER ${tableName}_temporal_trigger
        BEFORE INSERT OR UPDATE OR DELETE ON ${fullTableName}
        FOR EACH ROW EXECUTE FUNCTION ${schema}.${triggerFunctionName}();
      `);

      // Create view for current data only
      await client.query(`
        CREATE OR REPLACE VIEW ${schema}.${tableName}_current AS
        SELECT * FROM ${fullTableName}
        WHERE valid_to = '9999-12-31 23:59:59';
      `);

      await client.query('COMMIT');

      console.log(`Temporal table ${fullTableName} created successfully`);
    } catch (error) {
      await client.query('ROLLBACK');
      throw error;
    } finally {
      client.release();
    }
  }

  async queryAtTime(
    tableName: string,
    timestamp: Date,
    schema: string = 'public'
  ): Promise<any[]> {
    const result = await this.pool.query(
      `SELECT * FROM ${schema}.${tableName}
       WHERE valid_from <= $1 AND valid_to > $1`,
      [timestamp]
    );
    return result.rows;
  }

  async queryTimeRange(
    tableName: string,
    fromTime: Date,
    toTime: Date,
    schema: string = 'public'
  ): Promise<any[]> {
    const result = await this.pool.query(
      `SELECT * FROM ${schema}.${tableName}
       WHERE valid_from >= $1 AND valid_to <= $2
       ORDER BY valid_from DESC`,
      [fromTime, toTime]
    );
    return result.rows;
  }

  async getRecordHistory(
    tableName: string,
    recordId: string,
    schema: string = 'public'
  ): Promise<TemporalRecord[]> {
    const historyTableName = `${tableName}_history`;

    const result = await this.pool.query(
      `SELECT *,
              'UPDATE'::varchar as operation
       FROM ${schema}.${historyTableName}
       WHERE id = $1
       UNION ALL
       SELECT *,
              CASE
                WHEN valid_to = '9999-12-31 23:59:59' THEN 'INSERT'::varchar
                ELSE 'UPDATE'::varchar
              END as operation
       FROM ${schema}.${tableName}
       WHERE id = $1
       ORDER BY transaction_time DESC, version DESC`,
      [recordId]
    );

    return result.rows.map(row => ({
      id: row.id,
      data: row,
      validFromTime: row.valid_from,
      validToTime: row.valid_to,
      transactionTime: row.transaction_time,
      version: row.version,
      operation: row.operation,
    }));
  }

  async restoreFromTime(
    tableName: string,
    recordId: string,
    timestamp: Date,
    schema: string = 'public'
  ): Promise<any> {
    const result = await this.pool.query(
      `SELECT * FROM ${schema}.${tableName}
       WHERE id = $1 AND valid_from <= $2 AND valid_to > $2`,
      [recordId, timestamp]
    );

    if (result.rows.length === 0) {
      throw new Error(`No record found for ID ${recordId} at timestamp ${timestamp}`);
    }

    return result.rows[0];
  }

  async bulkRestore(
    tableName: string,
    timestamp: Date,
    schema: string = 'public'
  ): Promise<any[]> {
    const result = await this.pool.query(
      `SELECT * FROM ${schema}.${tableName}
       WHERE valid_from <= $1 AND valid_to > $1
       ORDER BY id`,
      [timestamp]
    );

    return result.rows;
  }

  async archiveOldRecords(
    tableName: string,
    archiveBefore: Date,
    schema: string = 'public'
  ): Promise<number> {
    const client = await this.pool.connect();
    try {
      await client.query('BEGIN');

      const historyTableName = `${tableName}_history`;

      // Archive old versions
      const result = await client.query(
        `DELETE FROM ${schema}.${historyTableName}
         WHERE archived_at < $1
         RETURNING *`,
        [archiveBefore]
      );

      const deletedCount = result.rowCount || 0;

      // Vacuum the table
      await client.query(`VACUUM ANALYZE ${schema}.${historyTableName}`);

      await client.query('COMMIT');

      console.log(`Archived ${deletedCount} records from ${historyTableName}`);
      return deletedCount;
    } catch (error) {
      await client.query('ROLLBACK');
      throw error;
    } finally {
      client.release();
    }
  }

  async getVersionDiff(
    tableName: string,
    recordId: string,
    version1: number,
    version2: number,
    schema: string = 'public'
  ): Promise<any> {
    const historyTableName = `${tableName}_history`;

    const result = await this.pool.query(
      `SELECT
        v1 as old_version,
        v2 as new_version,
        jsonb_diff_text(to_jsonb(v1), to_jsonb(v2)) as differences
       FROM (
         SELECT
           (SELECT row_to_json(t) FROM (SELECT * FROM ${schema}.${historyTableName} WHERE id = $1 AND version = $2) t) as v1,
           (SELECT row_to_json(t) FROM (SELECT * FROM ${schema}.${historyTableName} WHERE id = $1 AND version = $3) t) as v2
       ) diffs`,
      [recordId, version1, version2]
    );

    return result.rows[0] || {};
  }

  async createTemporalIndex(
    tableName: string,
    columns: string[],
    schema: string = 'public'
  ): Promise<void> {
    const columnsList = columns.join(',');
    const indexName = `idx_${tableName}_${columns.join('_')}_temporal`;

    await this.pool.query(`
      CREATE INDEX IF NOT EXISTS ${indexName}
      ON ${schema}.${tableName}(${columnsList}, valid_from, valid_to)
      WHERE valid_to = '9999-12-31 23:59:59';
    `);

    console.log(`Temporal index ${indexName} created`);
  }

  async analyzeTemporalPerformance(
    tableName: string,
    schema: string = 'public'
  ): Promise<any> {
    const fullTableName = `${schema}.${tableName}`;
    const historyTableName = `${fullTableName}_history`;

    const result = await this.pool.query(`
      SELECT
        (SELECT count(*) FROM ${fullTableName}) as current_records,
        (SELECT count(*) FROM ${historyTableName}) as history_records,
        (SELECT avg(version) FROM ${fullTableName}) as avg_versions,
        (SELECT max(version) FROM ${fullTableName}) as max_versions,
        (SELECT sum(pg_total_relation_size(t::regclass))
         FROM (VALUES ('${fullTableName}'::regclass), ('${historyTableName}'::regclass)) t(t)
        ) as total_size_bytes;
    `);

    return result.rows[0];
  }
}
