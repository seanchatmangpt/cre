/**
 * Canary Release Controller
 * Manages gradual rollout of new versions with automatic rollback on anomalies
 */

export interface CanaryConfig {
  initialPercentage: number;
  targetPercentage: number;
  incrementPercentage: number;
  incrementInterval: number; // milliseconds
  maxDuration: number; // milliseconds
  metricsWindow: number; // milliseconds for collecting metrics
}

export interface CanaryMetrics {
  errorRate: number;
  latency: number; // p99 latency in ms
  throughput: number; // requests/second
  cpuUsage: number; // percentage
  memoryUsage: number; // percentage
  customMetrics: Record<string, number>;
  timestamp: Date;
}

export interface CanaryThresholds {
  maxErrorRate: number;
  maxLatency: number; // milliseconds
  minThroughput: number; // requests/second
  maxCpuUsage: number; // percentage
  maxMemoryUsage: number; // percentage
  customMetricThresholds: Record<string, { max?: number; min?: number }>;
}

export interface CanaryPhase {
  phase: number;
  percentage: number;
  startTime: Date;
  endTime?: Date;
  status: 'pending' | 'active' | 'completed' | 'failed' | 'rolled_back';
  metrics: CanaryMetrics[];
  anomalies: CanaryAnomaly[];
}

export interface CanaryAnomaly {
  type: 'error_rate' | 'latency' | 'throughput' | 'custom';
  severity: 'warning' | 'critical';
  threshold: number;
  actual: number;
  timestamp: Date;
  message: string;
}

export interface CanaryRelease {
  id: string;
  version: string;
  status: 'pending' | 'running' | 'completed' | 'failed' | 'rolled_back';
  baseline: DeploymentBaseline;
  config: CanaryConfig;
  thresholds: CanaryThresholds;
  phases: CanaryPhase[];
  startTime: Date;
  endTime?: Date;
  events: CanaryEvent[];
}

export interface DeploymentBaseline {
  version: string;
  errorRate: number;
  latency: number;
  throughput: number;
}

export interface CanaryEvent {
  timestamp: Date;
  type: 'phase_started' | 'phase_completed' | 'anomaly_detected' | 'rollback_triggered' | 'canary_completed';
  phase: number;
  details: Record<string, unknown>;
}

/**
 * Canary Release Controller
 * Manages gradual rollout with automated anomaly detection and rollback
 */
export class CanaryReleaseController {
  private releases: Map<string, CanaryRelease> = new Map();
  private metricsCollectors: Map<string, NodeJS.Timer> = new Map();
  private anomalyDetectors: Map<string, AnomalyDetector> = new Map();

  /**
   * Start a new canary release
   */
  async startCanaryRelease(
    version: string,
    baselineVersion: string,
    config: CanaryConfig,
    thresholds: CanaryThresholds,
  ): Promise<CanaryRelease> {
    const releaseId = `canary-${version}-${Date.now()}`;

    console.log(`[CANARY] Starting canary release for v${version}`);

    const release: CanaryRelease = {
      id: releaseId,
      version,
      status: 'running',
      baseline: {
        version: baselineVersion,
        errorRate: 0.02,
        latency: 100,
        throughput: 1000,
      },
      config,
      thresholds,
      phases: [],
      startTime: new Date(),
      events: [],
    };

    this.releases.set(releaseId, release);

    // Start metrics collection
    this.startMetricsCollection(releaseId, release);

    // Start canary phases
    try {
      await this.executeCanaryPhases(releaseId, release);
      release.status = 'completed';
      release.endTime = new Date();
      this.recordEvent(releaseId, release, 'canary_completed', { version });
      console.log(`[CANARY] Canary release v${version} completed successfully`);
    } catch (error) {
      release.status = 'failed';
      release.endTime = new Date();
      console.error(`[CANARY] Canary release v${version} failed: ${error}`);
      throw error;
    } finally {
      this.stopMetricsCollection(releaseId);
    }

    return release;
  }

  /**
   * Get canary release status
   */
  getCanaryStatus(releaseId: string): CanaryRelease | undefined {
    return this.releases.get(releaseId);
  }

  /**
   * Get all active canary releases
   */
  getActiveCanaries(): CanaryRelease[] {
    return Array.from(this.releases.values()).filter(r => r.status === 'running');
  }

  /**
   * Analyze current metrics against thresholds
   */
  analyzeMetrics(releaseId: string, metrics: CanaryMetrics): CanaryAnomaly[] {
    const release = this.releases.get(releaseId);
    if (!release) return [];

    const anomalies: CanaryAnomaly[] = [];
    const thresholds = release.thresholds;
    const baseline = release.baseline;

    // Check error rate
    if (metrics.errorRate > thresholds.maxErrorRate) {
      anomalies.push({
        type: 'error_rate',
        severity: this.calculateSeverity(metrics.errorRate, thresholds.maxErrorRate),
        threshold: thresholds.maxErrorRate,
        actual: metrics.errorRate,
        timestamp: new Date(),
        message: `Error rate ${(metrics.errorRate * 100).toFixed(2)}% exceeds threshold ${(thresholds.maxErrorRate * 100).toFixed(2)}%`,
      });
    }

    // Check latency
    if (metrics.latency > thresholds.maxLatency) {
      anomalies.push({
        type: 'latency',
        severity: this.calculateSeverity(metrics.latency, thresholds.maxLatency),
        threshold: thresholds.maxLatency,
        actual: metrics.latency,
        timestamp: new Date(),
        message: `P99 latency ${metrics.latency}ms exceeds threshold ${thresholds.maxLatency}ms`,
      });
    }

    // Check throughput
    if (metrics.throughput < thresholds.minThroughput) {
      anomalies.push({
        type: 'throughput',
        severity: 'warning',
        threshold: thresholds.minThroughput,
        actual: metrics.throughput,
        timestamp: new Date(),
        message: `Throughput ${metrics.throughput} req/s below threshold ${thresholds.minThroughput} req/s`,
      });
    }

    // Check resource usage
    if (metrics.cpuUsage > thresholds.maxCpuUsage) {
      anomalies.push({
        type: 'custom',
        severity: 'warning',
        threshold: thresholds.maxCpuUsage,
        actual: metrics.cpuUsage,
        timestamp: new Date(),
        message: `CPU usage ${metrics.cpuUsage.toFixed(2)}% exceeds threshold ${thresholds.maxCpuUsage}%`,
      });
    }

    if (metrics.memoryUsage > thresholds.maxMemoryUsage) {
      anomalies.push({
        type: 'custom',
        severity: 'warning',
        threshold: thresholds.maxMemoryUsage,
        actual: metrics.memoryUsage,
        timestamp: new Date(),
        message: `Memory usage ${metrics.memoryUsage.toFixed(2)}% exceeds threshold ${thresholds.maxMemoryUsage}%`,
      });
    }

    // Check custom metrics
    for (const [metricName, thresholdConfig] of Object.entries(thresholds.customMetricThresholds)) {
      const metricValue = metrics.customMetrics[metricName];
      if (metricValue !== undefined) {
        if (thresholdConfig.max !== undefined && metricValue > thresholdConfig.max) {
          anomalies.push({
            type: 'custom',
            severity: 'warning',
            threshold: thresholdConfig.max,
            actual: metricValue,
            timestamp: new Date(),
            message: `Custom metric ${metricName} ${metricValue} exceeds threshold ${thresholdConfig.max}`,
          });
        }
        if (thresholdConfig.min !== undefined && metricValue < thresholdConfig.min) {
          anomalies.push({
            type: 'custom',
            severity: 'warning',
            threshold: thresholdConfig.min,
            actual: metricValue,
            timestamp: new Date(),
            message: `Custom metric ${metricName} ${metricValue} below threshold ${thresholdConfig.min}`,
          });
        }
      }
    }

    return anomalies;
  }

  /**
   * Manually trigger rollback of canary release
   */
  async rollbackCanary(releaseId: string, reason: string): Promise<void> {
    const release = this.releases.get(releaseId);
    if (!release) {
      throw new Error(`Canary release ${releaseId} not found`);
    }

    console.log(`[CANARY] Rolling back canary release ${releaseId}: ${reason}`);

    release.status = 'rolled_back';
    release.endTime = new Date();

    this.recordEvent(releaseId, release, 'rollback_triggered', { reason });
    this.stopMetricsCollection(releaseId);
  }

  // Private methods

  private async executeCanaryPhases(releaseId: string, release: CanaryRelease): Promise<void> {
    const config = release.config;
    let currentPercentage = config.initialPercentage;
    let phaseNumber = 1;

    while (currentPercentage <= config.targetPercentage) {
      const phase: CanaryPhase = {
        phase: phaseNumber,
        percentage: currentPercentage,
        startTime: new Date(),
        status: 'active',
        metrics: [],
        anomalies: [],
      };

      release.phases.push(phase);
      console.log(`[CANARY] Starting phase ${phaseNumber} - ${currentPercentage}% traffic to v${release.version}`);
      this.recordEvent(releaseId, release, 'phase_started', { phase: phaseNumber, percentage: currentPercentage });

      // Run phase for configured duration
      const phaseStartTime = Date.now();
      while (Date.now() - phaseStartTime < config.incrementInterval) {
        if (release.status === 'rolled_back') {
          return;
        }

        // Simulate metric collection and anomaly detection
        await this.sleep(1000);

        // Check for anomalies
        if (this.anomalyDetectors.has(releaseId)) {
          const detector = this.anomalyDetectors.get(releaseId)!;
          const anomalies = detector.getDetectedAnomalies();

          if (anomalies.length > 0) {
            const criticalAnomalies = anomalies.filter(a => a.severity === 'critical');
            if (criticalAnomalies.length > 0) {
              console.error(`[CANARY] Critical anomalies detected in phase ${phaseNumber}`);
              phase.status = 'failed';
              phase.anomalies = anomalies;
              throw new Error(`Critical anomalies detected: ${criticalAnomalies.map(a => a.message).join(', ')}`);
            }
          }
        }
      }

      phase.status = 'completed';
      phase.endTime = new Date();
      this.recordEvent(releaseId, release, 'phase_completed', { phase: phaseNumber });

      currentPercentage = Math.min(currentPercentage + config.incrementPercentage, config.targetPercentage);
      phaseNumber++;
    }

    console.log(`[CANARY] All canary phases completed for v${release.version}`);
  }

  private startMetricsCollection(releaseId: string, release: CanaryRelease): void {
    // Create anomaly detector
    const detector = new AnomalyDetector(release.baseline, release.thresholds);
    this.anomalyDetectors.set(releaseId, detector);

    // Start collecting metrics
    const interval = setInterval(() => {
      const metrics = this.collectMetrics();
      const anomalies = this.analyzeMetrics(releaseId, metrics);

      if (release.phases.length > 0) {
        const currentPhase = release.phases[release.phases.length - 1];
        currentPhase.metrics.push(metrics);
        currentPhase.anomalies.push(...anomalies);
      }

      if (anomalies.length > 0) {
        const criticalAnomalies = anomalies.filter(a => a.severity === 'critical');
        if (criticalAnomalies.length > 0) {
          this.recordEvent(releaseId, release, 'anomaly_detected', {
            anomalies: criticalAnomalies,
          });
        }
      }
    }, release.config.metricsWindow);

    this.metricsCollectors.set(releaseId, interval);
  }

  private stopMetricsCollection(releaseId: string): void {
    if (this.metricsCollectors.has(releaseId)) {
      clearInterval(this.metricsCollectors.get(releaseId)!);
      this.metricsCollectors.delete(releaseId);
    }
    this.anomalyDetectors.delete(releaseId);
  }

  private collectMetrics(): CanaryMetrics {
    // Simulate metric collection from monitoring system
    return {
      errorRate: Math.random() * 0.1,
      latency: Math.floor(Math.random() * 200) + 50,
      throughput: Math.floor(Math.random() * 500) + 500,
      cpuUsage: Math.random() * 80,
      memoryUsage: Math.random() * 70,
      customMetrics: {
        databaseLatency: Math.floor(Math.random() * 100) + 20,
        cacheHitRate: Math.random() * 0.9 + 0.1,
      },
      timestamp: new Date(),
    };
  }

  private calculateSeverity(actual: number, threshold: number): 'warning' | 'critical' {
    // Critical if 50% over threshold
    return actual > threshold * 1.5 ? 'critical' : 'warning';
  }

  private recordEvent(releaseId: string, release: CanaryRelease, type: CanaryEvent['type'], details: Record<string, unknown>): void {
    release.events.push({
      timestamp: new Date(),
      type,
      phase: release.phases.length,
      details,
    });
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

/**
 * Anomaly Detector
 * Detects deviations from baseline using statistical methods
 */
class AnomalyDetector {
  private detectedAnomalies: CanaryAnomaly[] = [];
  private baselineMetrics: CanaryMetrics[] = [];

  constructor(private baseline: DeploymentBaseline, private thresholds: CanaryThresholds) {}

  /**
   * Get detected anomalies
   */
  getDetectedAnomalies(): CanaryAnomaly[] {
    return this.detectedAnomalies;
  }

  /**
   * Reset detected anomalies
   */
  resetAnomalies(): void {
    this.detectedAnomalies = [];
  }

  /**
   * Add baseline metrics for comparison
   */
  addBaselineMetric(metrics: CanaryMetrics): void {
    this.baselineMetrics.push(metrics);
    if (this.baselineMetrics.length > 100) {
      this.baselineMetrics.shift();
    }
  }

  /**
   * Detect anomalies using statistical methods
   */
  detectAnomalies(metrics: CanaryMetrics): CanaryAnomaly[] {
    const anomalies: CanaryAnomaly[] = [];

    // Compare to baseline
    const errorRateDelta = (metrics.errorRate - this.baseline.errorRate) / this.baseline.errorRate;
    if (errorRateDelta > 0.5) {
      anomalies.push({
        type: 'error_rate',
        severity: errorRateDelta > 1 ? 'critical' : 'warning',
        threshold: this.baseline.errorRate,
        actual: metrics.errorRate,
        timestamp: new Date(),
        message: `Error rate increased ${(errorRateDelta * 100).toFixed(0)}% above baseline`,
      });
    }

    const latencyDelta = (metrics.latency - this.baseline.latency) / this.baseline.latency;
    if (latencyDelta > 0.5) {
      anomalies.push({
        type: 'latency',
        severity: latencyDelta > 1 ? 'critical' : 'warning',
        threshold: this.baseline.latency,
        actual: metrics.latency,
        timestamp: new Date(),
        message: `Latency increased ${(latencyDelta * 100).toFixed(0)}% above baseline`,
      });
    }

    return anomalies;
  }
}
