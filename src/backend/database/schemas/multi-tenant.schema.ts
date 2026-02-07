import { Pool } from 'pg';

export interface Tenant {
  id: string;
  name: string;
  schema: string;
  createdAt: Date;
  updatedAt: Date;
  isActive: boolean;
  metadata?: Record<string, any>;
}

export interface TenantContext {
  tenantId: string;
  schema: string;
  userId?: string;
  permissions?: string[];
}

export class MultiTenantSchemaManager {
  private pool: Pool;
  private schemaPrefix: string = 'tenant_';

  constructor(pool: Pool) {
    this.pool = pool;
  }

  async initializeTenantInfrastructure(): Promise<void> {
    const client = await this.pool.connect();
    try {
      // Create tenants table in public schema
      await client.query(`
        CREATE TABLE IF NOT EXISTS public.tenants (
          id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
          name VARCHAR(255) NOT NULL UNIQUE,
          schema_name VARCHAR(255) NOT NULL UNIQUE,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          is_active BOOLEAN DEFAULT true,
          metadata JSONB,
          CONSTRAINT valid_schema_name CHECK (schema_name ~ '^[a-z0-9_]+$')
        );
      `);

      // Create index on active tenants
      await client.query(`
        CREATE INDEX IF NOT EXISTS idx_tenants_active
        ON public.tenants(is_active);
      `);

      // Create tenant audit log
      await client.query(`
        CREATE TABLE IF NOT EXISTS public.tenant_audit_log (
          id BIGSERIAL PRIMARY KEY,
          tenant_id UUID NOT NULL REFERENCES public.tenants(id) ON DELETE CASCADE,
          action VARCHAR(50) NOT NULL,
          details JSONB,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          created_by VARCHAR(255)
        );
      `);

      // Create index on tenant audit log
      await client.query(`
        CREATE INDEX IF NOT EXISTS idx_tenant_audit_log_tenant_id
        ON public.tenant_audit_log(tenant_id, created_at DESC);
      `);

      console.log('Multi-tenant infrastructure initialized successfully');
    } finally {
      client.release();
    }
  }

  async createTenant(name: string, metadata?: Record<string, any>): Promise<Tenant> {
    const client = await this.pool.connect();
    try {
      await client.query('BEGIN');

      const schemaName = `${this.schemaPrefix}${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

      // Insert tenant record
      const tenantResult = await client.query(
        `INSERT INTO public.tenants (name, schema_name, metadata)
         VALUES ($1, $2, $3)
         RETURNING id, name, schema_name as schema, created_at, updated_at, is_active, metadata`,
        [name, schemaName, metadata || {}]
      );

      const tenant = tenantResult.rows[0];

      // Create tenant schema
      await client.query(`CREATE SCHEMA ${schemaName}`);

      // Create tenant-specific tables
      await this.createTenantTables(client, schemaName);

      // Enable RLS (Row Level Security)
      await this.enableRowLevelSecurity(client, schemaName);

      await client.query('COMMIT');

      // Log the creation
      await this.logTenantAudit(tenant.id, 'CREATED', { schema: schemaName });

      return tenant;
    } catch (error) {
      await client.query('ROLLBACK');
      throw error;
    } finally {
      client.release();
    }
  }

  private async createTenantTables(client: any, schemaName: string): Promise<void> {
    // Create tenant-specific data table
    await client.query(`
      CREATE TABLE IF NOT EXISTS ${schemaName}.data (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        table_name VARCHAR(255) NOT NULL,
        record_id UUID NOT NULL,
        data JSONB NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        created_by UUID,
        updated_by UUID,
        UNIQUE(table_name, record_id)
      );
    `);

    // Create indexes
    await client.query(`
      CREATE INDEX IF NOT EXISTS idx_${schemaName}_data_table_name
      ON ${schemaName}.data(table_name);
    `);

    await client.query(`
      CREATE INDEX IF NOT EXISTS idx_${schemaName}_data_created_at
      ON ${schemaName}.data(created_at DESC);
    `);
  }

  private async enableRowLevelSecurity(client: any, schemaName: string): Promise<void> {
    // Enable RLS on tenant data table
    await client.query(`
      ALTER TABLE ${schemaName}.data ENABLE ROW LEVEL SECURITY;
    `);

    // Create RLS policy - users can only see data they created
    await client.query(`
      CREATE POLICY user_data_isolation ON ${schemaName}.data
      FOR SELECT
      USING (created_by = current_user_id());
    `);

    // Create RLS policy for updates
    await client.query(`
      CREATE POLICY user_data_update ON ${schemaName}.data
      FOR UPDATE
      USING (created_by = current_user_id());
    `);
  }

  async getTenant(tenantId: string): Promise<Tenant | null> {
    const result = await this.pool.query(
      `SELECT id, name, schema_name as schema, created_at, updated_at, is_active, metadata
       FROM public.tenants WHERE id = $1`,
      [tenantId]
    );

    return result.rows[0] || null;
  }

  async getTenantByName(name: string): Promise<Tenant | null> {
    const result = await this.pool.query(
      `SELECT id, name, schema_name as schema, created_at, updated_at, is_active, metadata
       FROM public.tenants WHERE name = $1`,
      [name]
    );

    return result.rows[0] || null;
  }

  async listTenants(active?: boolean): Promise<Tenant[]> {
    let query = `SELECT id, name, schema_name as schema, created_at, updated_at, is_active, metadata
                 FROM public.tenants`;
    const params: any[] = [];

    if (active !== undefined) {
      query += ` WHERE is_active = $1`;
      params.push(active);
    }

    query += ` ORDER BY created_at DESC`;

    const result = await this.pool.query(query, params);
    return result.rows;
  }

  async updateTenant(tenantId: string, updates: Partial<Tenant>): Promise<Tenant> {
    const { name, isActive, metadata } = updates;

    const result = await this.pool.query(
      `UPDATE public.tenants
       SET name = COALESCE($1, name),
           is_active = COALESCE($2, is_active),
           metadata = COALESCE($3, metadata),
           updated_at = CURRENT_TIMESTAMP
       WHERE id = $4
       RETURNING id, name, schema_name as schema, created_at, updated_at, is_active, metadata`,
      [name, isActive, metadata, tenantId]
    );

    await this.logTenantAudit(tenantId, 'UPDATED', updates);

    return result.rows[0];
  }

  async deleteTenant(tenantId: string, cascade: boolean = false): Promise<void> {
    const client = await this.pool.connect();
    try {
      await client.query('BEGIN');

      const tenant = await this.getTenant(tenantId);
      if (!tenant) throw new Error(`Tenant ${tenantId} not found`);

      if (cascade) {
        // Drop tenant schema and all contents
        await client.query(`DROP SCHEMA IF EXISTS ${tenant.schema} CASCADE`);
      }

      // Mark as inactive instead of deleting for audit trail
      await client.query(
        `UPDATE public.tenants SET is_active = false, updated_at = CURRENT_TIMESTAMP WHERE id = $1`,
        [tenantId]
      );

      await this.logTenantAudit(tenantId, 'DELETED', { cascade });

      await client.query('COMMIT');
    } catch (error) {
      await client.query('ROLLBACK');
      throw error;
    } finally {
      client.release();
    }
  }

  private async logTenantAudit(
    tenantId: string,
    action: string,
    details?: any,
    userId?: string
  ): Promise<void> {
    await this.pool.query(
      `INSERT INTO public.tenant_audit_log (tenant_id, action, details, created_by)
       VALUES ($1, $2, $3, $4)`,
      [tenantId, action, JSON.stringify(details || {}), userId || 'system']
    );
  }

  async createTenantContext(tenantId: string, userId?: string): Promise<TenantContext> {
    const tenant = await this.getTenant(tenantId);
    if (!tenant || !tenant.isActive) {
      throw new Error(`Tenant ${tenantId} not found or inactive`);
    }

    return {
      tenantId: tenant.id,
      schema: tenant.schema,
      userId,
    };
  }

  async switchTenantSchema(client: any, tenantContext: TenantContext): Promise<void> {
    await client.query(`SET search_path TO ${tenantContext.schema}, public`);

    // Set context variables for RLS
    if (tenantContext.userId) {
      await client.query(`SELECT set_config('app.current_user_id', $1, false)`, [tenantContext.userId]);
    }
  }
}
