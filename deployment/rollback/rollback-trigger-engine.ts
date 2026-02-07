/**
 * Rollback Trigger Engine
 * Detects issues and automatically triggers rollback when thresholds are breached
 */

export type TriggerType = 'metric' | 'health' | 'error_spike' | 'dependency_failure' | 'manual';
export type RollbackPriority = 'low' | 'medium' | 'high' | 'critical';
export type TriggerState = 'armed' | 'triggered' | 'cooldown' | 'disabled';

export interface MetricThreshold {
  metric: string;
  operator: 'gt' | 'lt' | 'gte' | 'lte' | 'eq' | 'ne';
  value: number;
  duration: number; // milliseconds the condition must persist
  cooldown: number; // milliseconds before allowing another trigger
}

export interface HealthCheckTrigger {
  serviceName: string;
  checkType: 'readiness' | 'liveness' | 'startup';
  failureThreshold: number; // number of consecutive failures
  checkInterval: number; // milliseconds
  cooldown: number;
}

export interface ErrorSpikeTrigger {
  baselineErrorRate: number;
  increasePercentage: number; // 50 = 50% increase above baseline
  windowSize: number; // milliseconds to evaluate
  minErrorCount: number; // minimum errors to trigger
  cooldown: number;
}

export interface DependencyFailureTrigger {
  dependencyName: string;
  failureTimeWindow: number; // milliseconds
  requiredFailureCount: number;
  cooldown: number;
}

export interface RollbackTrigger {
  id: string;
  type: TriggerType;
  condition: MetricThreshold | HealthCheckTrigger | ErrorSpikeTrigger | DependencyFailureTrigger;
  state: TriggerState;
  priority: RollbackPriority;
  enabled: boolean;
  lastTriggeredAt?: Date;
  cooldownUntil?: Date;
  evaluations: TriggerEvaluation[];
}

export interface TriggerEvaluation {
  timestamp: Date;
  conditionMet: boolean;
  value?: number;
  threshold?: number;
  message: string;
}

export interface RollbackEvent {
  id: string;
  timestamp: Date;
  triggeredBy: RollbackTrigger[];
  rollbackVersion: string;
  currentVersion: string;
  reason: string;
  priority: RollbackPriority;
  status: 'initiated' | 'in_progress' | 'completed' | 'failed';
  executionTime?: number;
  result?: string;
}

/**
 * Rollback Trigger Engine
 * Monitors system health and automatically triggers rollbacks
 */
export class RollbackTriggerEngine {
  private triggers: Map<string, RollbackTrigger> = new Map();
  private evaluationIntervals: Map<string, NodeJS.Timer> = new Map();
  private rollbackHistory: RollbackEvent[] = [];
  private metricsStore: Map<string, number[]> = new Map();
  private healthCheckStore: Map<string, HealthCheckResult[]> = new Map();
  private errorStore: Map<string, ErrorEvent[]> = new Map();

  private rollbackCallback?: (event: RollbackEvent) => Promise<void>;

  /**
   * Register a metric threshold trigger
   */
  registerMetricTrigger(
    id: string,
    threshold: MetricThreshold,
    priority: RollbackPriority = 'medium',
  ): RollbackTrigger {
    const trigger: RollbackTrigger = {
      id,
      type: 'metric',
      condition: threshold,
      state: 'armed',
      priority,
      enabled: true,
      evaluations: [],
    };

    this.triggers.set(id, trigger);
    this.startMetricEvaluation(id, trigger);
    return trigger;
  }

  /**
   * Register a health check trigger
   */
  registerHealthCheckTrigger(
    id: string,
    healthCheck: HealthCheckTrigger,
    priority: RollbackPriority = 'medium',
  ): RollbackTrigger {
    const trigger: RollbackTrigger = {
      id,
      type: 'health',
      condition: healthCheck,
      state: 'armed',
      priority,
      enabled: true,
      evaluations: [],
    };

    this.triggers.set(id, trigger);
    this.startHealthCheckEvaluation(id, trigger);
    return trigger;
  }

  /**
   * Register an error spike trigger
   */
  registerErrorSpikeTrigger(
    id: string,
    errorSpike: ErrorSpikeTrigger,
    priority: RollbackPriority = 'high',
  ): RollbackTrigger {
    const trigger: RollbackTrigger = {
      id,
      type: 'error_spike',
      condition: errorSpike,
      state: 'armed',
      priority,
      enabled: true,
      evaluations: [],
    };

    this.triggers.set(id, trigger);
    this.startErrorSpikeEvaluation(id, trigger);
    return trigger;
  }

  /**
   * Register a dependency failure trigger
   */
  registerDependencyFailureTrigger(
    id: string,
    depFailure: DependencyFailureTrigger,
    priority: RollbackPriority = 'high',
  ): RollbackTrigger {
    const trigger: RollbackTrigger = {
      id,
      type: 'dependency_failure',
      condition: depFailure,
      state: 'armed',
      priority,
      enabled: true,
      evaluations: [],
    };

    this.triggers.set(id, trigger);
    this.startDependencyFailureEvaluation(id, trigger);
    return trigger;
  }

  /**
   * Record a metric value for evaluation
   */
  recordMetric(metricName: string, value: number): void {
    if (!this.metricsStore.has(metricName)) {
      this.metricsStore.set(metricName, []);
    }
    const values = this.metricsStore.get(metricName)!;
    values.push(value);

    // Keep only recent values (last 5 minutes worth at 1 sample/second)
    if (values.length > 300) {
      values.shift();
    }
  }

  /**
   * Record a health check result
   */
  recordHealthCheck(serviceName: string, result: HealthCheckResult): void {
    if (!this.healthCheckStore.has(serviceName)) {
      this.healthCheckStore.set(serviceName, []);
    }
    const results = this.healthCheckStore.get(serviceName)!;
    results.push(result);

    // Keep only recent results (last 50)
    if (results.length > 50) {
      results.shift();
    }
  }

  /**
   * Record an error event
   */
  recordError(serviceName: string, error: ErrorEvent): void {
    if (!this.errorStore.has(serviceName)) {
      this.errorStore.set(serviceName, []);
    }
    const errors = this.errorStore.get(serviceName)!;
    errors.push(error);

    // Keep only recent errors (last 5 minutes)
    const cutoff = Date.now() - 5 * 60 * 1000;
    while (errors.length > 0 && errors[0].timestamp.getTime() < cutoff) {
      errors.shift();
    }
  }

  /**
   * Set callback for rollback events
   */
  onRollback(callback: (event: RollbackEvent) => Promise<void>): void {
    this.rollbackCallback = callback;
  }

  /**
   * Manually trigger rollback
   */
  async triggerRollback(currentVersion: string, rollbackVersion: string, reason: string): Promise<RollbackEvent> {
    const triggeredTriggers = Array.from(this.triggers.values()).filter(t => t.enabled);

    const event: RollbackEvent = {
      id: `rollback-${Date.now()}`,
      timestamp: new Date(),
      triggeredBy: triggeredTriggers,
      currentVersion,
      rollbackVersion,
      reason,
      priority: 'critical',
      status: 'initiated',
    };

    this.rollbackHistory.push(event);
    console.log(`[ROLLBACK] Triggering rollback: ${reason}`);
    console.log(`[ROLLBACK] Current: v${currentVersion} -> Rollback: v${rollbackVersion}`);

    const startTime = Date.now();
    try {
      if (this.rollbackCallback) {
        await this.rollbackCallback(event);
      }

      event.status = 'completed';
      event.executionTime = Date.now() - startTime;
      console.log(`[ROLLBACK] Rollback completed in ${event.executionTime}ms`);
    } catch (error) {
      event.status = 'failed';
      event.executionTime = Date.now() - startTime;
      event.result = String(error);
      console.error(`[ROLLBACK] Rollback failed: ${error}`);
    }

    return event;
  }

  /**
   * Get rollback trigger by ID
   */
  getTrigger(id: string): RollbackTrigger | undefined {
    return this.triggers.get(id);
  }

  /**
   * Get all triggers
   */
  getAllTriggers(): RollbackTrigger[] {
    return Array.from(this.triggers.values());
  }

  /**
   * Disable a trigger
   */
  disableTrigger(id: string): void {
    const trigger = this.triggers.get(id);
    if (trigger) {
      trigger.enabled = false;
      console.log(`[TRIGGER] Disabled trigger ${id}`);
    }
  }

  /**
   * Enable a trigger
   */
  enableTrigger(id: string): void {
    const trigger = this.triggers.get(id);
    if (trigger) {
      trigger.enabled = true;
      trigger.state = 'armed';
      console.log(`[TRIGGER] Enabled trigger ${id}`);
    }
  }

  /**
   * Get rollback history
   */
  getRollbackHistory(limit: number = 50): RollbackEvent[] {
    return this.rollbackHistory.slice(-limit);
  }

  /**
   * Stop all evaluations
   */
  stopAllEvaluations(): void {
    for (const [, interval] of this.evaluationIntervals) {
      clearInterval(interval);
    }
    this.evaluationIntervals.clear();
  }

  // Private methods

  private startMetricEvaluation(id: string, trigger: RollbackTrigger): void {
    const threshold = trigger.condition as MetricThreshold;
    const checkInterval = 5000; // Check every 5 seconds

    const interval = setInterval(() => {
      if (!trigger.enabled) return;

      // Skip if in cooldown
      if (trigger.cooldownUntil && Date.now() < trigger.cooldownUntil.getTime()) {
        trigger.state = 'cooldown';
        return;
      }

      const metrics = this.metricsStore.get(threshold.metric) || [];
      const recentMetrics = metrics.slice(-(Math.ceil(threshold.duration / 1000)));

      if (recentMetrics.length === 0) {
        return;
      }

      const allConditionMet = recentMetrics.every(value => this.evaluateCondition(value, threshold));

      const evaluation: TriggerEvaluation = {
        timestamp: new Date(),
        conditionMet: allConditionMet,
        value: recentMetrics[recentMetrics.length - 1],
        threshold: threshold.value,
        message: `Metric ${threshold.metric}: ${recentMetrics[recentMetrics.length - 1]} ${threshold.operator} ${threshold.value}`,
      };

      trigger.evaluations.push(evaluation);
      if (trigger.evaluations.length > 100) {
        trigger.evaluations.shift();
      }

      if (allConditionMet && trigger.state === 'armed') {
        console.error(`[TRIGGER] Metric trigger ${id} activated: ${evaluation.message}`);
        trigger.state = 'triggered';
        trigger.lastTriggeredAt = new Date();
        trigger.cooldownUntil = new Date(Date.now() + threshold.cooldown);
      }
    }, checkInterval);

    this.evaluationIntervals.set(id, interval);
  }

  private startHealthCheckEvaluation(id: string, trigger: RollbackTrigger): void {
    const healthCheck = trigger.condition as HealthCheckTrigger;

    const interval = setInterval(() => {
      if (!trigger.enabled) return;

      // Skip if in cooldown
      if (trigger.cooldownUntil && Date.now() < trigger.cooldownUntil.getTime()) {
        trigger.state = 'cooldown';
        return;
      }

      const results = this.healthCheckStore.get(healthCheck.serviceName) || [];
      const recentResults = results.slice(-healthCheck.failureThreshold);

      if (recentResults.length < healthCheck.failureThreshold) {
        return;
      }

      const allFailed = recentResults.every(r => !r.healthy);

      const evaluation: TriggerEvaluation = {
        timestamp: new Date(),
        conditionMet: allFailed,
        message: `Health check for ${healthCheck.serviceName}: ${allFailed ? 'FAILED' : 'OK'}`,
      };

      trigger.evaluations.push(evaluation);
      if (trigger.evaluations.length > 100) {
        trigger.evaluations.shift();
      }

      if (allFailed && trigger.state === 'armed') {
        console.error(`[TRIGGER] Health check trigger ${id} activated: ${evaluation.message}`);
        trigger.state = 'triggered';
        trigger.lastTriggeredAt = new Date();
        trigger.cooldownUntil = new Date(Date.now() + healthCheck.cooldown);
      }
    }, healthCheck.checkInterval);

    this.evaluationIntervals.set(id, interval);
  }

  private startErrorSpikeEvaluation(id: string, trigger: RollbackTrigger): void {
    const errorSpike = trigger.condition as ErrorSpikeTrigger;

    const interval = setInterval(() => {
      if (!trigger.enabled) return;

      // Skip if in cooldown
      if (trigger.cooldownUntil && Date.now() < trigger.cooldownUntil.getTime()) {
        trigger.state = 'cooldown';
        return;
      }

      const errors = this.errorStore.get('application') || [];
      const now = Date.now();
      const windowErrors = errors.filter(e => now - e.timestamp.getTime() < errorSpike.windowSize);

      const totalRequests = Math.max(100, windowErrors.length * 10); // Estimate
      const errorRate = windowErrors.length / totalRequests;
      const expectedRate = errorSpike.baselineErrorRate;
      const increase = (errorRate - expectedRate) / expectedRate;

      const evaluation: TriggerEvaluation = {
        timestamp: new Date(),
        conditionMet: increase > (errorSpike.increasePercentage / 100) && windowErrors.length >= errorSpike.minErrorCount,
        value: errorRate * 100,
        threshold: expectedRate * (1 + errorSpike.increasePercentage / 100) * 100,
        message: `Error rate spike: ${(errorRate * 100).toFixed(2)}% (${increase > 0 ? '+' : ''}${(increase * 100).toFixed(0)}%)`,
      };

      trigger.evaluations.push(evaluation);
      if (trigger.evaluations.length > 100) {
        trigger.evaluations.shift();
      }

      if (evaluation.conditionMet && trigger.state === 'armed') {
        console.error(`[TRIGGER] Error spike trigger ${id} activated: ${evaluation.message}`);
        trigger.state = 'triggered';
        trigger.lastTriggeredAt = new Date();
        trigger.cooldownUntil = new Date(Date.now() + errorSpike.cooldown);
      }
    }, 5000);

    this.evaluationIntervals.set(id, interval);
  }

  private startDependencyFailureEvaluation(id: string, trigger: RollbackTrigger): void {
    const depFailure = trigger.condition as DependencyFailureTrigger;

    const interval = setInterval(() => {
      if (!trigger.enabled) return;

      // Skip if in cooldown
      if (trigger.cooldownUntil && Date.now() < trigger.cooldownUntil.getTime()) {
        trigger.state = 'cooldown';
        return;
      }

      const errors = this.errorStore.get(depFailure.dependencyName) || [];
      const now = Date.now();
      const windowErrors = errors.filter(e => now - e.timestamp.getTime() < depFailure.failureTimeWindow);

      const evaluation: TriggerEvaluation = {
        timestamp: new Date(),
        conditionMet: windowErrors.length >= depFailure.requiredFailureCount,
        value: windowErrors.length,
        threshold: depFailure.requiredFailureCount,
        message: `Dependency ${depFailure.dependencyName}: ${windowErrors.length} failures in window`,
      };

      trigger.evaluations.push(evaluation);
      if (trigger.evaluations.length > 100) {
        trigger.evaluations.shift();
      }

      if (evaluation.conditionMet && trigger.state === 'armed') {
        console.error(`[TRIGGER] Dependency failure trigger ${id} activated: ${evaluation.message}`);
        trigger.state = 'triggered';
        trigger.lastTriggeredAt = new Date();
        trigger.cooldownUntil = new Date(Date.now() + depFailure.cooldown);
      }
    }, 5000);

    this.evaluationIntervals.set(id, interval);
  }

  private evaluateCondition(value: number, threshold: MetricThreshold): boolean {
    switch (threshold.operator) {
      case 'gt':
        return value > threshold.value;
      case 'lt':
        return value < threshold.value;
      case 'gte':
        return value >= threshold.value;
      case 'lte':
        return value <= threshold.value;
      case 'eq':
        return value === threshold.value;
      case 'ne':
        return value !== threshold.value;
      default:
        return false;
    }
  }
}

export interface HealthCheckResult {
  timestamp: Date;
  healthy: boolean;
  responseTime: number;
  details?: string;
}

export interface ErrorEvent {
  timestamp: Date;
  code: string;
  message: string;
  stackTrace?: string;
}
