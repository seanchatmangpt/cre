/**
 * Deployment System Integration Tests
 * Tests for zero-downtime deployment components
 */

import { VersionCompatibilityValidator } from './validation/version-compatibility';
import { BlueGreenOrchestrator } from './blue-green/blue-green-orchestrator';
import { CanaryReleaseController } from './canary/canary-release-controller';
import { RollbackTriggerEngine } from './rollback/rollback-trigger-engine';
import { DeploymentOrchestrator } from './deployment-orchestrator';

describe('Version Compatibility Validator', () => {
  let validator: VersionCompatibilityValidator;

  beforeEach(() => {
    validator = new VersionCompatibilityValidator('1.0.0');
  });

  test('should validate compatible versions', () => {
    const validation = validator.validateUpgrade('1.1.0');
    expect(validation.isValid).toBe(true);
    expect(validation.errors).toHaveLength(0);
  });

  test('should detect incompatible versions', () => {
    const validation = validator.validateUpgrade('999.0.0');
    expect(validation.isValid).toBe(false);
    expect(validation.errors.length).toBeGreaterThan(0);
  });

  test('should identify breaking changes', () => {
    const validation = validator.validateUpgrade('1.2.0');
    expect(validation.warnings.length).toBeGreaterThan(0);
    expect(validation.warnings.some(w => w.code === 'BREAKING_CHANGES')).toBe(true);
  });

  test('should generate pre-upgrade steps', () => {
    const validation = validator.validateUpgrade('1.1.0');
    expect(validation.preUpgradeSteps.length).toBeGreaterThan(0);
    expect(validation.preUpgradeSteps).toContain('Backup current database');
  });

  test('should generate post-upgrade steps', () => {
    const validation = validator.validateUpgrade('1.1.0');
    expect(validation.postUpgradeSteps.length).toBeGreaterThan(0);
    expect(validation.postUpgradeSteps).toContain('Run smoke tests');
  });

  test('should estimate upgrade duration', () => {
    const validation = validator.validateUpgrade('1.1.0');
    expect(validation.estimatedDuration).toBeGreaterThan(0);
    expect(validation.estimatedDuration).toBeLessThan(200); // Should be reasonable
  });
});

describe('Blue-Green Orchestrator', () => {
  let orchestrator: BlueGreenOrchestrator;

  beforeEach(() => {
    orchestrator = new BlueGreenOrchestrator(
      {
        name: 'blue',
        replicas: 3,
        resources: { cpu: '1000m', memory: '2Gi' },
        healthCheckInterval: 5000,
        readinessProbe: {
          path: '/health/ready',
          initialDelaySeconds: 10,
          timeoutSeconds: 5,
          periodSeconds: 5,
          successThreshold: 1,
          failureThreshold: 3,
        },
        livenessProbe: {
          path: '/health/live',
          initialDelaySeconds: 15,
          timeoutSeconds: 5,
          periodSeconds: 10,
          successThreshold: 1,
          failureThreshold: 3,
        },
        tags: { deployment: 'blue' },
      },
      {
        name: 'green',
        replicas: 3,
        resources: { cpu: '1000m', memory: '2Gi' },
        healthCheckInterval: 5000,
        readinessProbe: {
          path: '/health/ready',
          initialDelaySeconds: 10,
          timeoutSeconds: 5,
          periodSeconds: 5,
          successThreshold: 1,
          failureThreshold: 3,
        },
        livenessProbe: {
          path: '/health/live',
          initialDelaySeconds: 15,
          timeoutSeconds: 5,
          periodSeconds: 10,
          successThreshold: 1,
          failureThreshold: 3,
        },
        tags: { deployment: 'green' },
      },
    );
  });

  test('should deploy to inactive environment', async () => {
    const deployment = await orchestrator.deployToInactiveEnvironment('1.1.0');
    expect(deployment).toBeDefined();
    expect(deployment.version).toBe('1.1.0');
    expect(deployment.status).toBe('ready');
  });

  test('should switch traffic immediately', async () => {
    await orchestrator.deployToInactiveEnvironment('1.1.0');
    await orchestrator.switchTraffic({ strategy: 'immediate', rollbackOnError: false });

    const status = orchestrator.getStatus();
    expect(status.active).toBe('green');
  });

  test('should track deployment events', async () => {
    await orchestrator.deployToInactiveEnvironment('1.1.0');
    const status = orchestrator.getStatus();
    expect(status.events.length).toBeGreaterThan(0);
    expect(status.events[0].type).toBe('deployment_started');
  });

  test('should validate health before traffic switch', async () => {
    const deployment = await orchestrator.deployToInactiveEnvironment('1.1.0');
    const status = orchestrator.getStatus();
    expect(deployment.healthStatus.isHealthy).toBe(true);
  });

  test('should support gradual traffic switching', async () => {
    await orchestrator.deployToInactiveEnvironment('1.1.0');
    await orchestrator.switchTraffic({
      strategy: 'gradual',
      percentageIncrement: 25,
      incrementInterval: 1000,
      rollbackOnError: false,
    });

    const status = orchestrator.getStatus();
    expect(status.active).toBe('green');
  });

  test('should rollback to previous environment', async () => {
    await orchestrator.deployToInactiveEnvironment('1.1.0');
    await orchestrator.switchTraffic({ strategy: 'immediate', rollbackOnError: false });

    let status = orchestrator.getStatus();
    expect(status.active).toBe('green');

    await orchestrator.rollback();

    status = orchestrator.getStatus();
    expect(status.active).toBe('blue');
  });
});

describe('Canary Release Controller', () => {
  let controller: CanaryReleaseController;

  beforeEach(() => {
    controller = new CanaryReleaseController();
  });

  test('should start canary release', async () => {
    const release = await controller.startCanaryRelease(
      '1.1.0',
      '1.0.0',
      {
        initialPercentage: 10,
        targetPercentage: 100,
        incrementPercentage: 10,
        incrementInterval: 5000,
        maxDuration: 60000,
        metricsWindow: 2000,
      },
      {
        maxErrorRate: 0.05,
        maxLatency: 2000,
        minThroughput: 500,
        maxCpuUsage: 80,
        maxMemoryUsage: 85,
        customMetricThresholds: {},
      },
    );

    expect(release).toBeDefined();
    expect(release.version).toBe('1.1.0');
    expect(release.status).toBe('completed');
  });

  test('should get active canaries', async () => {
    const release1 = controller.startCanaryRelease(
      '1.1.0',
      '1.0.0',
      {
        initialPercentage: 10,
        targetPercentage: 50,
        incrementPercentage: 10,
        incrementInterval: 1000,
        maxDuration: 10000,
        metricsWindow: 500,
      },
      {
        maxErrorRate: 0.05,
        maxLatency: 2000,
        minThroughput: 500,
        maxCpuUsage: 80,
        maxMemoryUsage: 85,
        customMetricThresholds: {},
      },
    );

    const canaries = controller.getActiveCanaries();
    expect(canaries.length).toBeGreaterThan(0);
  });

  test('should analyze metrics for anomalies', () => {
    const metrics = {
      errorRate: 0.10,
      latency: 3000,
      throughput: 1000,
      cpuUsage: 50,
      memoryUsage: 60,
      customMetrics: {},
      timestamp: new Date(),
    };

    const thresholds = {
      maxErrorRate: 0.05,
      maxLatency: 2000,
      minThroughput: 500,
      maxCpuUsage: 80,
      maxMemoryUsage: 85,
      customMetricThresholds: {},
    };

    const anomalies = controller.analyzeMetrics('fake-id', metrics);
    expect(anomalies.length).toBeGreaterThan(0);
    expect(anomalies.some(a => a.type === 'error_rate')).toBe(true);
    expect(anomalies.some(a => a.type === 'latency')).toBe(true);
  });

  test('should detect critical anomalies', () => {
    const metrics = {
      errorRate: 0.20, // Well over threshold
      latency: 5000, // Well over threshold
      throughput: 1000,
      cpuUsage: 50,
      memoryUsage: 60,
      customMetrics: {},
      timestamp: new Date(),
    };

    const anomalies = controller.analyzeMetrics('fake-id', metrics);
    const criticalAnomalies = anomalies.filter(a => a.severity === 'critical');
    expect(criticalAnomalies.length).toBeGreaterThan(0);
  });
});

describe('Rollback Trigger Engine', () => {
  let engine: RollbackTriggerEngine;

  beforeEach(() => {
    engine = new RollbackTriggerEngine();
  });

  test('should register metric trigger', () => {
    const trigger = engine.registerMetricTrigger('test-trigger', {
      metric: 'error_rate',
      operator: 'gt',
      value: 0.10,
      duration: 30000,
      cooldown: 60000,
    });

    expect(trigger).toBeDefined();
    expect(trigger.id).toBe('test-trigger');
    expect(trigger.type).toBe('metric');
    expect(trigger.enabled).toBe(true);
  });

  test('should register health check trigger', () => {
    const trigger = engine.registerHealthCheckTrigger('health-trigger', {
      serviceName: 'app',
      checkType: 'readiness',
      failureThreshold: 3,
      checkInterval: 5000,
      cooldown: 60000,
    });

    expect(trigger).toBeDefined();
    expect(trigger.type).toBe('health');
  });

  test('should register error spike trigger', () => {
    const trigger = engine.registerErrorSpikeTrigger('error-spike', {
      baselineErrorRate: 0.02,
      increasePercentage: 100,
      windowSize: 60000,
      minErrorCount: 10,
      cooldown: 120000,
    });

    expect(trigger).toBeDefined();
    expect(trigger.type).toBe('error_spike');
  });

  test('should record metrics', () => {
    engine.recordMetric('error_rate', 0.05);
    engine.recordMetric('error_rate', 0.06);

    const trigger = engine.registerMetricTrigger('test', {
      metric: 'error_rate',
      operator: 'gt',
      value: 0.05,
      duration: 5000,
      cooldown: 60000,
    });

    expect(trigger).toBeDefined();
  });

  test('should disable triggers', () => {
    const trigger = engine.registerMetricTrigger('test', {
      metric: 'error_rate',
      operator: 'gt',
      value: 0.10,
      duration: 30000,
      cooldown: 60000,
    });

    engine.disableTrigger('test');
    const disabledTrigger = engine.getTrigger('test');

    expect(disabledTrigger?.enabled).toBe(false);
  });

  test('should trigger rollback manually', async () => {
    let rollbackTriggered = false;
    engine.onRollback(async () => {
      rollbackTriggered = true;
    });

    await engine.triggerRollback('1.1.0', '1.0.0', 'Manual trigger');
    expect(rollbackTriggered).toBe(true);
  });

  test('should track rollback history', async () => {
    await engine.triggerRollback('1.1.0', '1.0.0', 'Manual trigger');
    const history = engine.getRollbackHistory();

    expect(history.length).toBeGreaterThan(0);
    expect(history[0].currentVersion).toBe('1.1.0');
    expect(history[0].rollbackVersion).toBe('1.0.0');
  });
});

describe('Deployment Orchestrator', () => {
  let orchestrator: DeploymentOrchestrator;

  beforeEach(() => {
    orchestrator = new DeploymentOrchestrator('1.0.0');
  });

  test('should create deployment plan', async () => {
    const plan = await orchestrator.createDeploymentPlan({
      strategy: 'blue-green',
      version: '1.1.0',
      targetServices: ['app', 'api'],
      blueGreenConfig: {
        replicasPerEnv: 3,
        healthCheckInterval: 5000,
      },
      notificationChannels: [],
      rollbackOnError: true,
      maxDeploymentDuration: 600000,
    });

    expect(plan).toBeDefined();
    expect(plan.config.version).toBe('1.1.0');
    expect(plan.status).toBe('pending');
    expect(plan.phases.length).toBe(5);
  });

  test('should validate version before planning', async () => {
    const plan = await orchestrator.createDeploymentPlan({
      strategy: 'blue-green',
      version: '1.1.0',
      targetServices: ['app'],
      notificationChannels: [],
      rollbackOnError: true,
      maxDeploymentDuration: 600000,
    });

    expect(plan.validationResult.isValid).toBe(true);
  });

  test('should generate pre/post-upgrade steps', async () => {
    const plan = await orchestrator.createDeploymentPlan({
      strategy: 'blue-green',
      version: '1.1.0',
      targetServices: ['app'],
      notificationChannels: [],
      rollbackOnError: true,
      maxDeploymentDuration: 600000,
    });

    expect(plan.validationResult.preUpgradeSteps.length).toBeGreaterThan(0);
    expect(plan.validationResult.postUpgradeSteps.length).toBeGreaterThan(0);
  });

  test('should track deployment phases', async () => {
    const plan = await orchestrator.createDeploymentPlan({
      strategy: 'blue-green',
      version: '1.1.0',
      targetServices: ['app'],
      notificationChannels: [],
      rollbackOnError: true,
      maxDeploymentDuration: 600000,
    });

    const expectedPhases = ['validation', 'preparation', 'deployment', 'monitoring', 'completion'];
    const actualPhases = plan.phases.map(p => p.phase);

    expect(actualPhases).toEqual(expectedPhases);
  });
});

describe('Integration Tests', () => {
  test('should complete full deployment workflow', async () => {
    const orchestrator = new DeploymentOrchestrator('1.0.0');

    // Create plan
    const plan = await orchestrator.createDeploymentPlan({
      strategy: 'blue-green',
      version: '1.1.0',
      targetServices: ['app'],
      blueGreenConfig: {
        replicasPerEnv: 3,
        healthCheckInterval: 5000,
      },
      trafficSwitchConfig: {
        strategy: 'immediate',
        rollbackOnError: true,
      },
      notificationChannels: [],
      rollbackOnError: true,
      maxDeploymentDuration: 600000,
    });

    expect(plan).toBeDefined();
    expect(plan.validationResult.isValid).toBe(true);
  });

  test('should validate and deploy canary', async () => {
    const validator = new VersionCompatibilityValidator('1.0.0');
    const controller = new CanaryReleaseController();

    const validation = validator.validateUpgrade('1.1.0');
    expect(validation.isValid).toBe(true);

    const release = await controller.startCanaryRelease(
      '1.1.0',
      '1.0.0',
      {
        initialPercentage: 10,
        targetPercentage: 100,
        incrementPercentage: 10,
        incrementInterval: 5000,
        maxDuration: 60000,
        metricsWindow: 2000,
      },
      {
        maxErrorRate: 0.05,
        maxLatency: 2000,
        minThroughput: 500,
        maxCpuUsage: 80,
        maxMemoryUsage: 85,
        customMetricThresholds: {},
      },
    );

    expect(release.status).toBe('completed');
  });

  test('should handle rollback in deployment', async () => {
    const engine = new RollbackTriggerEngine();
    let rollbackCalled = false;

    engine.onRollback(async () => {
      rollbackCalled = true;
    });

    await engine.triggerRollback('1.1.0', '1.0.0', 'Test rollback');
    expect(rollbackCalled).toBe(true);

    const history = engine.getRollbackHistory();
    expect(history.length).toBeGreaterThan(0);
  });
});
