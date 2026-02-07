/**
 * Version Compatibility Validation System
 * Ensures safe upgrades by validating version constraints and dependencies
 */

export interface VersionInfo {
  version: string;
  releaseDate: Date;
  compatibleVersions: string[];
  breakingChanges: string[];
  deprecatedFeatures: string[];
  requiredServices: ServiceDependency[];
  databaseMigrations: string[];
}

export interface ServiceDependency {
  name: string;
  minVersion: string;
  maxVersion?: string;
  required: boolean;
}

export interface UpgradeValidation {
  isValid: boolean;
  fromVersion: string;
  toVersion: string;
  errors: ValidationError[];
  warnings: ValidationWarning[];
  requiresDowntime: boolean;
  estimatedDuration: number;
  preUpgradeSteps: string[];
  postUpgradeSteps: string[];
}

export interface ValidationError {
  code: string;
  message: string;
  severity: 'critical' | 'error';
  resolution: string;
}

export interface ValidationWarning {
  code: string;
  message: string;
  severity: 'warning' | 'info';
  recommendation: string;
}

/**
 * Version Compatibility Validator
 * Validates upgrade paths and checks compatibility constraints
 */
export class VersionCompatibilityValidator {
  private versionRegistry: Map<string, VersionInfo>;
  private currentVersion: string;
  private currentServices: Map<string, string>;

  constructor(currentVersion: string) {
    this.currentVersion = currentVersion;
    this.versionRegistry = new Map();
    this.currentServices = new Map();
    this.initializeVersionRegistry();
  }

  /**
   * Register a service version for compatibility checking
   */
  registerService(serviceName: string, version: string): void {
    this.currentServices.set(serviceName, version);
  }

  /**
   * Validate upgrade from current version to target version
   */
  validateUpgrade(targetVersion: string): UpgradeValidation {
    const errors: ValidationError[] = [];
    const warnings: ValidationWarning[] = [];

    // Check if target version exists
    if (!this.versionRegistry.has(targetVersion)) {
      errors.push({
        code: 'VERSION_NOT_FOUND',
        message: `Target version ${targetVersion} not found in registry`,
        severity: 'critical',
        resolution: 'Check version number or register new version in version registry',
      });
      return this.createFailedValidation(targetVersion, errors);
    }

    const currentVersionInfo = this.versionRegistry.get(this.currentVersion);
    const targetVersionInfo = this.versionRegistry.get(targetVersion);

    if (!currentVersionInfo || !targetVersionInfo) {
      errors.push({
        code: 'VERSION_INFO_MISSING',
        message: `Version info missing for ${this.currentVersion} or ${targetVersion}`,
        severity: 'critical',
        resolution: 'Register version info in version registry',
      });
      return this.createFailedValidation(targetVersion, errors);
    }

    // Validate upgrade path
    const upgradePathValid = this.validateUpgradePath(this.currentVersion, targetVersion);
    if (!upgradePathValid) {
      errors.push({
        code: 'INCOMPATIBLE_VERSIONS',
        message: `Cannot upgrade from ${this.currentVersion} to ${targetVersion}`,
        severity: 'critical',
        resolution: `Valid upgrade paths from ${this.currentVersion}: ${currentVersionInfo.compatibleVersions.join(', ')}`,
      });
      return this.createFailedValidation(targetVersion, errors);
    }

    // Validate service dependencies
    const depErrors = this.validateServiceDependencies(targetVersionInfo);
    errors.push(...depErrors);

    // Check for breaking changes
    const breakingChanges = this.getBreakingChanges(this.currentVersion, targetVersion);
    if (breakingChanges.length > 0) {
      warnings.push({
        code: 'BREAKING_CHANGES',
        message: `${breakingChanges.length} breaking changes in target version`,
        severity: 'warning',
        recommendation: `Review breaking changes: ${breakingChanges.join('; ')}`,
      });
    }

    // Check for deprecated features
    const deprecated = this.getDeprecatedFeatures(this.currentVersion, targetVersion);
    if (deprecated.length > 0) {
      warnings.push({
        code: 'DEPRECATED_FEATURES',
        message: `${deprecated.length} features deprecated in target version`,
        severity: 'warning',
        recommendation: `Update code to remove deprecated features: ${deprecated.join(', ')}`,
      });
    }

    // Validate database migrations
    const migrationErrors = this.validateMigrations(targetVersionInfo.databaseMigrations);
    errors.push(...migrationErrors);

    return {
      isValid: errors.length === 0,
      fromVersion: this.currentVersion,
      toVersion: targetVersion,
      errors,
      warnings,
      requiresDowntime: this.requiresDowntime(this.currentVersion, targetVersion),
      estimatedDuration: this.estimateUpgradeDuration(this.currentVersion, targetVersion),
      preUpgradeSteps: this.generatePreUpgradeSteps(targetVersion),
      postUpgradeSteps: this.generatePostUpgradeSteps(targetVersion),
    };
  }

  /**
   * Validate upgrade path exists
   */
  private validateUpgradePath(fromVersion: string, toVersion: string): boolean {
    const versionInfo = this.versionRegistry.get(fromVersion);
    if (!versionInfo) return false;

    // Allow direct upgrade to compatible versions
    if (versionInfo.compatibleVersions.includes(toVersion)) {
      return true;
    }

    // Check if toVersion is newer and there's an upgrade chain
    if (this.isVersionNewer(toVersion, fromVersion)) {
      return this.hasUpgradeChain(fromVersion, toVersion);
    }

    return false;
  }

  /**
   * Validate service dependencies for target version
   */
  private validateServiceDependencies(targetVersionInfo: VersionInfo): ValidationError[] {
    const errors: ValidationError[] = [];

    for (const dep of targetVersionInfo.requiredServices) {
      if (!this.currentServices.has(dep.name)) {
        if (dep.required) {
          errors.push({
            code: 'MISSING_REQUIRED_SERVICE',
            message: `Required service ${dep.name} v${dep.minVersion} not found`,
            severity: 'critical',
            resolution: `Install ${dep.name} v${dep.minVersion} or higher before upgrading`,
          });
        }
        continue;
      }

      const serviceVersion = this.currentServices.get(dep.name)!;
      if (!this.versionSatisfies(serviceVersion, dep.minVersion, dep.maxVersion)) {
        errors.push({
          code: 'SERVICE_VERSION_INCOMPATIBLE',
          message: `Service ${dep.name} v${serviceVersion} does not meet requirement ${dep.minVersion}${dep.maxVersion ? ` to ${dep.maxVersion}` : ''}`,
          severity: 'error',
          resolution: `Update ${dep.name} to compatible version before upgrading`,
        });
      }
    }

    return errors;
  }

  /**
   * Validate database migrations
   */
  private validateMigrations(migrations: string[]): ValidationError[] {
    const errors: ValidationError[] = [];

    if (migrations.length === 0) {
      return errors;
    }

    // In a real implementation, check if migrations are safe and reversible
    for (const migration of migrations) {
      if (!this.isMigrationReversible(migration)) {
        errors.push({
          code: 'IRREVERSIBLE_MIGRATION',
          message: `Migration ${migration} cannot be automatically reversed`,
          severity: 'error',
          resolution: 'Ensure database backups exist before upgrading',
        });
      }
    }

    return errors;
  }

  /**
   * Get breaking changes between versions
   */
  private getBreakingChanges(fromVersion: string, toVersion: string): string[] {
    const targetVersionInfo = this.versionRegistry.get(toVersion);
    return targetVersionInfo?.breakingChanges || [];
  }

  /**
   * Get deprecated features between versions
   */
  private getDeprecatedFeatures(fromVersion: string, toVersion: string): string[] {
    const targetVersionInfo = this.versionRegistry.get(toVersion);
    return targetVersionInfo?.deprecatedFeatures || [];
  }

  /**
   * Check if upgrade requires downtime
   */
  private requiresDowntime(fromVersion: string, toVersion: string): boolean {
    const targetVersionInfo = this.versionRegistry.get(toVersion);
    if (!targetVersionInfo) return true;

    // Requires downtime if there are breaking changes or irreversible migrations
    return targetVersionInfo.breakingChanges.length > 0 ||
           targetVersionInfo.databaseMigrations.some(m => !this.isMigrationReversible(m));
  }

  /**
   * Estimate upgrade duration in minutes
   */
  private estimateUpgradeDuration(fromVersion: string, toVersion: string): number {
    const targetVersionInfo = this.versionRegistry.get(toVersion);
    if (!targetVersionInfo) return 60;

    let duration = 10; // Base duration

    // Add time for database migrations
    duration += targetVersionInfo.databaseMigrations.length * 5;

    // Add time for service restarts
    duration += targetVersionInfo.requiredServices.length * 3;

    return Math.max(10, duration);
  }

  /**
   * Generate pre-upgrade steps
   */
  private generatePreUpgradeSteps(targetVersion: string): string[] {
    const steps: string[] = [
      'Backup current database',
      'Document current configuration',
      'Run health checks on all services',
      'Notify stakeholders of upgrade window',
      'Verify rollback scripts are available',
      `Create snapshot of running version ${this.currentVersion}`,
    ];

    const targetVersionInfo = this.versionRegistry.get(targetVersion);
    if (targetVersionInfo && targetVersionInfo.databaseMigrations.length > 0) {
      steps.push('Review and test database migrations in staging');
    }

    if (targetVersionInfo && targetVersionInfo.breakingChanges.length > 0) {
      steps.push('Review breaking changes and update application code');
    }

    return steps;
  }

  /**
   * Generate post-upgrade steps
   */
  private generatePostUpgradeSteps(targetVersion: string): string[] {
    return [
      'Verify all services are running',
      'Run smoke tests',
      'Monitor application metrics',
      'Verify database integrity',
      'Check for any error logs',
      'Run end-to-end tests',
      'Notify stakeholders of upgrade completion',
      'Document upgrade results',
    ];
  }

  /**
   * Check if version satisfies constraints
   */
  private versionSatisfies(currentVersion: string, minVersion: string, maxVersion?: string): boolean {
    if (!this.isVersionNewer(currentVersion, minVersion) && currentVersion !== minVersion) {
      return false;
    }

    if (maxVersion && this.isVersionNewer(currentVersion, maxVersion)) {
      return false;
    }

    return true;
  }

  /**
   * Compare versions (simple semver comparison)
   */
  private isVersionNewer(v1: string, v2: string): boolean {
    const parts1 = v1.split('.').map(Number);
    const parts2 = v2.split('.').map(Number);

    for (let i = 0; i < Math.max(parts1.length, parts2.length); i++) {
      const p1 = parts1[i] || 0;
      const p2 = parts2[i] || 0;
      if (p1 > p2) return true;
      if (p1 < p2) return false;
    }
    return false;
  }

  /**
   * Check if upgrade chain exists
   */
  private hasUpgradeChain(fromVersion: string, toVersion: string): boolean {
    const visited = new Set<string>();
    const queue = [fromVersion];

    while (queue.length > 0) {
      const current = queue.shift()!;
      if (current === toVersion) return true;
      if (visited.has(current)) continue;

      visited.add(current);
      const versionInfo = this.versionRegistry.get(current);
      if (versionInfo) {
        queue.push(...versionInfo.compatibleVersions);
      }
    }

    return false;
  }

  /**
   * Check if migration is reversible
   */
  private isMigrationReversible(migration: string): boolean {
    // In a real system, check migration metadata
    return !migration.includes('DROP') && !migration.includes('TRUNCATE');
  }

  /**
   * Create failed validation result
   */
  private createFailedValidation(targetVersion: string, errors: ValidationError[]): UpgradeValidation {
    return {
      isValid: false,
      fromVersion: this.currentVersion,
      toVersion: targetVersion,
      errors,
      warnings: [],
      requiresDowntime: true,
      estimatedDuration: 0,
      preUpgradeSteps: [],
      postUpgradeSteps: [],
    };
  }

  /**
   * Initialize version registry with sample versions
   */
  private initializeVersionRegistry(): void {
    this.versionRegistry.set('1.0.0', {
      version: '1.0.0',
      releaseDate: new Date('2024-01-01'),
      compatibleVersions: ['1.1.0', '1.2.0'],
      breakingChanges: [],
      deprecatedFeatures: [],
      requiredServices: [
        { name: 'postgres', minVersion: '12.0', required: true },
        { name: 'redis', minVersion: '6.0', required: true },
      ],
      databaseMigrations: [],
    });

    this.versionRegistry.set('1.1.0', {
      version: '1.1.0',
      releaseDate: new Date('2024-06-01'),
      compatibleVersions: ['1.2.0', '2.0.0'],
      breakingChanges: [],
      deprecatedFeatures: ['legacy_api_v1'],
      requiredServices: [
        { name: 'postgres', minVersion: '12.0', required: true },
        { name: 'redis', minVersion: '6.0', required: true },
      ],
      databaseMigrations: ['001_add_users_table.sql', '002_add_indexes.sql'],
    });

    this.versionRegistry.set('1.2.0', {
      version: '1.2.0',
      releaseDate: new Date('2024-09-01'),
      compatibleVersions: ['2.0.0'],
      breakingChanges: ['removed_config_key_auth.legacy'],
      deprecatedFeatures: ['legacy_api_v1', 'legacy_auth_method'],
      requiredServices: [
        { name: 'postgres', minVersion: '13.0', required: true },
        { name: 'redis', minVersion: '6.2', required: true },
        { name: 'elasticsearch', minVersion: '7.0', required: false },
      ],
      databaseMigrations: ['003_update_schema.sql'],
    });

    this.versionRegistry.set('2.0.0', {
      version: '2.0.0',
      releaseDate: new Date('2025-01-15'),
      compatibleVersions: [],
      breakingChanges: [
        'config_structure_changed',
        'database_schema_v2',
        'api_response_format_changed',
      ],
      deprecatedFeatures: [],
      requiredServices: [
        { name: 'postgres', minVersion: '14.0', required: true },
        { name: 'redis', minVersion: '7.0', required: true },
        { name: 'elasticsearch', minVersion: '8.0', required: true },
      ],
      databaseMigrations: ['004_migrate_to_v2_schema.sql', '005_backfill_data.sql'],
    });
  }
}
