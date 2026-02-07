/**
 * Integration Test Runner Configuration
 * Orchestrates execution of all integration test suites
 * Provides utilities for test management and reporting
 */

export interface TestSuiteConfig {
  name: string;
  description: string;
  path: string;
  priority: 'critical' | 'high' | 'medium' | 'low';
  timeout: number;
  enabled: boolean;
  tags: string[];
}

export interface TestExecutionResult {
  suiteName: string;
  passed: number;
  failed: number;
  skipped: number;
  duration: number;
  timestamp: number;
  coverage: number;
}

export interface TestReportSummary {
  totalSuites: number;
  suitesPassed: number;
  suitesFailed: number;
  totalTests: number;
  testsPassed: number;
  testsFailed: number;
  totalDuration: number;
  averageCoverage: number;
  successRate: number;
}

/**
 * All integration test suites
 */
export const TEST_SUITES: TestSuiteConfig[] = [
  {
    name: 'Deployment Validation',
    description: 'Tests deployment readiness, system initialization, and resource availability',
    path: './deployment-validation.test.ts',
    priority: 'critical',
    timeout: 300000, // 5 minutes
    enabled: true,
    tags: ['deployment', 'validation', 'critical'],
  },
  {
    name: 'Upgrade Testing',
    description: 'Tests zero-downtime upgrades, schema migrations, and version compatibility',
    path: './upgrade-testing.test.ts',
    priority: 'critical',
    timeout: 600000, // 10 minutes
    enabled: true,
    tags: ['upgrade', 'migration', 'critical'],
  },
  {
    name: 'Failure Recovery',
    description: 'Tests resilience, failover mechanisms, and recovery from various failures',
    path: './failure-recovery.test.ts',
    priority: 'critical',
    timeout: 300000, // 5 minutes
    enabled: true,
    tags: ['failure', 'recovery', 'critical'],
  },
  {
    name: 'Performance Benchmarks',
    description: 'Tests throughput, latency, memory usage, and resource efficiency',
    path: './performance-benchmarks.test.ts',
    priority: 'high',
    timeout: 600000, // 10 minutes
    enabled: true,
    tags: ['performance', 'benchmarks', 'high'],
  },
  {
    name: 'Load Testing',
    description: 'Tests system behavior under various load conditions and stress scenarios',
    path: './load-testing.test.ts',
    priority: 'high',
    timeout: 1800000, // 30 minutes
    enabled: true,
    tags: ['load', 'stress', 'high'],
  },
  {
    name: 'E2E Integration',
    description: 'Tests complete workflows combining multiple systems and services',
    path: './e2e-integration.test.ts',
    priority: 'high',
    timeout: 300000, // 5 minutes
    enabled: true,
    tags: ['e2e', 'integration', 'high'],
  },
];

/**
 * Test suite execution modes
 */
export enum ExecutionMode {
  FULL = 'full',
  CRITICAL_ONLY = 'critical-only',
  QUICK = 'quick',
  CUSTOM = 'custom',
}

/**
 * Test runner configuration
 */
export class TestRunnerConfig {
  executionMode: ExecutionMode = ExecutionMode.FULL;
  parallel: boolean = true;
  maxWorkers: number = 4;
  failFast: boolean = false;
  generateReport: boolean = true;
  reportFormat: 'json' | 'html' | 'junit' = 'json';
  reportPath: string = './test-results';
  coverage: boolean = true;
  minCoveragePercent: number = 80;
  tags: string[] = [];

  /**
   * Get enabled test suites based on configuration
   */
  getEnabledSuites(): TestSuiteConfig[] {
    let suites = TEST_SUITES.filter(s => s.enabled);

    switch (this.executionMode) {
      case ExecutionMode.CRITICAL_ONLY:
        suites = suites.filter(s => s.priority === 'critical');
        break;
      case ExecutionMode.QUICK:
        suites = suites.filter(s => s.timeout <= 300000);
        break;
      case ExecutionMode.CUSTOM:
        if (this.tags.length > 0) {
          suites = suites.filter(s =>
            s.tags.some(tag => this.tags.includes(tag)),
          );
        }
        break;
    }

    return suites;
  }

  /**
   * Get total estimated runtime
   */
  getEstimatedDuration(): number {
    const suites = this.getEnabledSuites();
    if (this.parallel) {
      const maxTimeout = Math.max(...suites.map(s => s.timeout), 0);
      return maxTimeout + 60000; // Add buffer for setup/teardown
    } else {
      return suites.reduce((sum, s) => sum + s.timeout, 0) + 60000;
    }
  }
}

/**
 * Integration test utilities
 */
export class TestUtilities {
  /**
   * Wait for condition with timeout
   */
  static async waitFor(
    condition: () => boolean,
    timeout: number = 5000,
    interval: number = 100,
  ): Promise<void> {
    const startTime = Date.now();
    while (!condition()) {
      if (Date.now() - startTime > timeout) {
        throw new Error(`Timeout waiting for condition after ${timeout}ms`);
      }
      await new Promise(resolve => setTimeout(resolve, interval));
    }
  }

  /**
   * Generate unique test identifier
   */
  static generateTestId(prefix: string = 'test'): string {
    return `${prefix}-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  }

  /**
   * Measure execution time
   */
  static async measureTime<T>(
    fn: () => Promise<T>,
  ): Promise<{ result: T; duration: number }> {
    const startTime = performance.now();
    const result = await fn();
    const duration = performance.now() - startTime;
    return { result, duration };
  }

  /**
   * Retry with exponential backoff
   */
  static async retryWithBackoff<T>(
    fn: () => Promise<T>,
    maxAttempts: number = 3,
    initialDelay: number = 100,
  ): Promise<T> {
    let lastError: Error | null = null;
    for (let attempt = 0; attempt < maxAttempts; attempt++) {
      try {
        return await fn();
      } catch (error) {
        lastError = error as Error;
        const delay = initialDelay * Math.pow(2, attempt);
        await new Promise(resolve => setTimeout(resolve, delay));
      }
    }
    throw lastError || new Error('Max retry attempts exceeded');
  }

  /**
   * Create mock HTTP response
   */
  static createMockResponse(
    status: number = 200,
    data: any = {},
    headers: Record<string, string> = {},
  ): Response {
    return new Response(JSON.stringify(data), {
      status,
      headers: {
        'Content-Type': 'application/json',
        ...headers,
      },
    });
  }

  /**
   * Create mock error response
   */
  static createMockErrorResponse(
    status: number = 500,
    error: string = 'Internal Server Error',
  ): Response {
    return new Response(JSON.stringify({ error }), {
      status,
      headers: { 'Content-Type': 'application/json' },
    });
  }

  /**
   * Generate load profile for testing
   */
  static generateLoadProfile(users: number, duration: number) {
    return {
      users,
      duration,
      rampUp: Math.ceil(duration / 10), // 10% for ramp up
      steady: Math.ceil(duration * 0.8), // 80% steady state
      rampDown: Math.ceil(duration / 10), // 10% for ramp down
    };
  }

  /**
   * Calculate statistics from numeric array
   */
  static calculateStats(values: number[]) {
    if (values.length === 0) return null;

    const sorted = [...values].sort((a, b) => a - b);
    const sum = values.reduce((a, b) => a + b, 0);
    const avg = sum / values.length;

    const variance =
      values.reduce((sum, val) => sum + Math.pow(val - avg, 2), 0) /
      values.length;
    const stdDev = Math.sqrt(variance);

    const p50 = sorted[Math.floor(sorted.length * 0.5)];
    const p95 = sorted[Math.floor(sorted.length * 0.95)];
    const p99 = sorted[Math.floor(sorted.length * 0.99)];

    return {
      min: sorted[0],
      max: sorted[sorted.length - 1],
      avg,
      stdDev,
      p50,
      p95,
      p99,
      count: values.length,
    };
  }
}

/**
 * Test report generator
 */
export class TestReportGenerator {
  /**
   * Generate test summary report
   */
  static generateSummary(results: TestExecutionResult[]): TestReportSummary {
    return {
      totalSuites: results.length,
      suitesPassed: results.filter(r => r.failed === 0).length,
      suitesFailed: results.filter(r => r.failed > 0).length,
      totalTests: results.reduce((sum, r) => sum + r.passed + r.failed, 0),
      testsPassed: results.reduce((sum, r) => sum + r.passed, 0),
      testsFailed: results.reduce((sum, r) => sum + r.failed, 0),
      totalDuration: results.reduce((sum, r) => sum + r.duration, 0),
      averageCoverage:
        results.reduce((sum, r) => sum + r.coverage, 0) / results.length,
      successRate: this.calculateSuccessRate(results),
    };
  }

  private static calculateSuccessRate(results: TestExecutionResult[]): number {
    const totalTests = results.reduce((sum, r) => sum + r.passed + r.failed, 0);
    const passedTests = results.reduce((sum, r) => sum + r.passed, 0);
    return totalTests === 0 ? 0 : passedTests / totalTests;
  }

  /**
   * Format duration for display
   */
  static formatDuration(ms: number): string {
    if (ms < 1000) return `${ms.toFixed(0)}ms`;
    if (ms < 60000) return `${(ms / 1000).toFixed(2)}s`;
    return `${(ms / 60000).toFixed(2)}m`;
  }

  /**
   * Format percentage
   */
  static formatPercent(value: number): string {
    return `${(value * 100).toFixed(2)}%`;
  }
}

/**
 * Test data generators for common scenarios
 */
export class TestDataGenerator {
  static generateUser(overrides: any = {}) {
    return {
      id: `user-${Date.now()}`,
      email: `user${Date.now()}@example.com`,
      name: 'Test User',
      createdAt: new Date(),
      ...overrides,
    };
  }

  static generateResource(overrides: any = {}) {
    return {
      id: `resource-${Date.now()}`,
      name: 'Test Resource',
      status: 'active',
      createdAt: new Date(),
      ...overrides,
    };
  }

  static generateAuditLog(overrides: any = {}) {
    return {
      id: `audit-${Date.now()}`,
      userId: 'user-1',
      action: 'created',
      resource: 'resource',
      timestamp: new Date(),
      details: {},
      ...overrides,
    };
  }

  static generateBatch(count: number, generator: () => any) {
    return Array.from({ length: count }, () => generator());
  }
}

/**
 * Performance assertions
 */
export class PerformanceAssertions {
  static assertLatency(
    actual: number,
    max: number,
    context: string = '',
  ): void {
    if (actual > max) {
      throw new Error(
        `Latency assertion failed ${context}: ${actual}ms exceeds max ${max}ms`,
      );
    }
  }

  static assertThroughput(
    actual: number,
    min: number,
    context: string = '',
  ): void {
    if (actual < min) {
      throw new Error(
        `Throughput assertion failed ${context}: ${actual} below min ${min}`,
      );
    }
  }

  static assertMemory(
    actual: number,
    max: number,
    context: string = '',
  ): void {
    if (actual > max) {
      throw new Error(
        `Memory assertion failed ${context}: ${actual} bytes exceeds max ${max} bytes`,
      );
    }
  }

  static assertErrorRate(
    actual: number,
    max: number,
    context: string = '',
  ): void {
    if (actual > max) {
      throw new Error(
        `Error rate assertion failed ${context}: ${actual} exceeds max ${max}`,
      );
    }
  }
}
