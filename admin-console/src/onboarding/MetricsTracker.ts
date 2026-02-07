/**
 * Metrics Tracker
 * Tracks success metrics and analytics for onboarding process
 */

import {
  SuccessMetric,
  OnboardingMetrics,
  OnboardingStep,
  OnboardingState,
  TrainingMetrics,
  EmailMetrics,
  OnboardingAnalytics,
  EmailMetricDetail,
} from './types';

interface MetricPoint {
  timestamp: Date;
  value: number;
  userId: string;
}

export class MetricsTracker {
  private metrics: SuccessMetric[] = [];
  private userMetrics: Map<string, OnboardingMetrics> = new Map();
  private metricTimeseries: Map<string, MetricPoint[]> = new Map();
  private onboardingStates: Map<string, OnboardingState> = new Map();

  /**
   * Record a success metric
   */
  recordMetric(metric: SuccessMetric): void {
    this.metrics.push(metric);

    // Add to timeseries
    const key = `${metric.category}:${metric.metricName}`;
    if (typeof metric.value === 'number') {
      const timeseries = this.metricTimeseries.get(key) || [];
      timeseries.push({
        timestamp: metric.timestamp,
        value: metric.value,
        userId: metric.userId || 'unknown',
      });
      this.metricTimeseries.set(key, timeseries);
    }
  }

  /**
   * Record email open/click events
   */
  recordEmailEvent(
    userId: string,
    templateId: string,
    eventType: 'open' | 'click' | 'bounce'
  ): void {
    const metric: SuccessMetric = {
      id: `${userId}-${templateId}-${eventType}-${Date.now()}`,
      metricName: `email_${eventType}`,
      value: 1,
      unit: 'count',
      category: 'engagement',
      timestamp: new Date(),
      userId,
    };

    this.recordMetric(metric);
  }

  /**
   * Record step completion
   */
  recordStepCompletion(userId: string, step: OnboardingStep): void {
    const metric: SuccessMetric = {
      id: `${userId}-${step}-completion-${Date.now()}`,
      metricName: 'step_completed',
      value: step,
      unit: 'step',
      category: 'completion',
      timestamp: new Date(),
      userId,
    };

    this.recordMetric(metric);
  }

  /**
   * Record feature usage
   */
  recordFeatureUsage(userId: string, featureName: string, usageCount: number = 1): void {
    const metric: SuccessMetric = {
      id: `${userId}-${featureName}-${Date.now()}`,
      metricName: 'feature_usage',
      value: usageCount,
      unit: 'count',
      category: 'feature_usage',
      timestamp: new Date(),
      userId,
    };

    this.recordMetric(metric);
  }

  /**
   * Initialize metrics for a user
   */
  initializeUserMetrics(userId: string, organizationId: string): OnboardingMetrics {
    const userMetrics: OnboardingMetrics = {
      userId,
      organizationId,
      startDate: new Date(),
      completionDate: undefined,
      timeToComplete: undefined,
      emailOpenRate: 0,
      stepCompletionRates: new Map(),
      featuresUsed: [],
      trainingCompletionRate: 0,
      overallHealthScore: 0,
      recommendations: [],
    };

    this.userMetrics.set(userId, userMetrics);
    return userMetrics;
  }

  /**
   * Update user metrics based on onboarding state
   */
  updateUserMetrics(userId: string, state: OnboardingState): OnboardingMetrics {
    let metrics = this.userMetrics.get(userId);
    if (!metrics) {
      metrics = this.initializeUserMetrics(userId, state.configuration.customSettings?.organizationId as string || 'unknown');
    }

    // Update completion metrics
    const allSteps = Object.values(OnboardingStep);
    const stepCompletionRates = new Map<OnboardingStep, number>();

    allSteps.forEach((step) => {
      const isCompleted = state.completedSteps.includes(step);
      stepCompletionRates.set(step, isCompleted ? 100 : 0);
    });

    metrics.stepCompletionRates = stepCompletionRates;

    if (state.completedAt) {
      metrics.completionDate = state.completedAt;
      const timeInMinutes = (state.completedAt.getTime() - state.startedAt.getTime()) / (1000 * 60);
      metrics.timeToComplete = Math.round(timeInMinutes);
    }

    // Calculate health score
    metrics.overallHealthScore = this.calculateHealthScore(metrics);

    // Generate recommendations
    metrics.recommendations = this.generateRecommendations(metrics);

    this.userMetrics.set(userId, metrics);
    return metrics;
  }

  /**
   * Calculate overall health score (0-100)
   */
  private calculateHealthScore(metrics: OnboardingMetrics): number {
    let score = 0;

    // Email engagement (20%)
    const emailScore = Math.min(100, metrics.emailOpenRate);
    score += (emailScore / 100) * 20;

    // Step completion (50%)
    const completedSteps = Array.from(metrics.stepCompletionRates.values()).filter((v) => v === 100).length;
    const totalSteps = metrics.stepCompletionRates.size;
    const stepScore = totalSteps > 0 ? (completedSteps / totalSteps) * 100 : 0;
    score += (stepScore / 100) * 50;

    // Training completion (20%)
    score += (metrics.trainingCompletionRate / 100) * 20;

    // Feature adoption (10%)
    const featureScore = Math.min(100, metrics.featuresUsed.length * 10);
    score += (featureScore / 100) * 10;

    return Math.round(score);
  }

  /**
   * Generate recommendations based on metrics
   */
  private generateRecommendations(metrics: OnboardingMetrics): string[] {
    const recommendations: string[] = [];

    if (metrics.emailOpenRate < 50) {
      recommendations.push('Consider improving email content to increase open rates');
    }

    const incompleteSteps = Array.from(metrics.stepCompletionRates.entries())
      .filter(([_, rate]) => rate === 0)
      .map(([step, _]) => step);

    if (incompleteSteps.length > 0) {
      recommendations.push(
        `User has not completed: ${incompleteSteps.join(', ')}. Consider reaching out.`
      );
    }

    if (metrics.trainingCompletionRate < 30) {
      recommendations.push('Encourage user to complete more training materials');
    }

    if (metrics.featuresUsed.length === 0) {
      recommendations.push('User has not used any features yet. Provide guided tours or support');
    }

    if (metrics.timeToComplete && metrics.timeToComplete > 480) {
      // More than 8 hours
      recommendations.push('Onboarding took longer than expected. Consider simplifying the process');
    }

    return recommendations;
  }

  /**
   * Get user metrics
   */
  getUserMetrics(userId: string): OnboardingMetrics | undefined {
    return this.userMetrics.get(userId);
  }

  /**
   * Get analytics dashboard data
   */
  getAnalytics(): OnboardingAnalytics {
    const allUsers = Array.from(this.userMetrics.values());
    const totalStarted = allUsers.length;
    const totalCompleted = allUsers.filter((m) => m.completionDate !== undefined).length;
    const completionRate = totalStarted > 0 ? (totalCompleted / totalStarted) * 100 : 0;

    const completionTimes = allUsers
      .filter((m) => m.timeToComplete !== undefined)
      .map((m) => m.timeToComplete || 0);

    const averageTimeToCompletion =
      completionTimes.length > 0
        ? completionTimes.reduce((a, b) => a + b, 0) / completionTimes.length
        : 0;

    // Calculate step dropoff rates
    const stepDropoffRates = new Map<OnboardingStep, number>();
    const allSteps = Object.values(OnboardingStep);

    allSteps.forEach((step) => {
      const usersWhoReachedStep = allUsers.filter((m) =>
        Array.from(m.stepCompletionRates.entries())
          .slice(0, allSteps.indexOf(step) + 1)
          .some(([_, rate]) => rate === 100)
      ).length;

      const usersWhoCompletedStep = allUsers.filter(
        (m) => (m.stepCompletionRates.get(step) || 0) === 100
      ).length;

      const dropoffRate =
        usersWhoReachedStep > 0
          ? ((usersWhoReachedStep - usersWhoCompletedStep) / usersWhoReachedStep) * 100
          : 0;

      stepDropoffRates.set(step, dropoffRate);
    });

    // Email metrics
    const emailMetrics = this.calculateEmailMetrics();

    // Training metrics
    const trainingMetrics = this.calculateTrainingMetrics();

    // Most common issues
    const commonIssuesMap = new Map<string, number>();
    allUsers.forEach((user) => {
      user.recommendations.forEach((rec) => {
        commonIssuesMap.set(rec, (commonIssuesMap.get(rec) || 0) + 1);
      });
    });

    const mostCommonIssues = Array.from(commonIssuesMap.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 5)
      .map(([issue, _]) => issue);

    return {
      totalUsersStarted: totalStarted,
      totalUsersCompleted: totalCompleted,
      completionRate: Math.round(completionRate),
      averageTimeToCompletion: Math.round(averageTimeToCompletion),
      stepDropoffRates,
      mostCommonIssues,
      emailEngagementMetrics: emailMetrics,
      trainingMetrics,
    };
  }

  /**
   * Calculate email engagement metrics
   */
  private calculateEmailMetrics(): EmailMetrics {
    const emailEvents = this.metrics.filter((m) => m.category === 'engagement');
    const totalSent = this.metrics.filter((m) => m.metricName === 'email_sent').length;

    const openCount = emailEvents.filter((m) => m.metricName === 'email_open').length;
    const clickCount = emailEvents.filter((m) => m.metricName === 'email_click').length;
    const bounceCount = emailEvents.filter((m) => m.metricName === 'email_bounce').length;

    const openRate = totalSent > 0 ? (openCount / totalSent) * 100 : 0;
    const clickRate = totalSent > 0 ? (clickCount / totalSent) * 100 : 0;
    const bounceRate = totalSent > 0 ? (bounceCount / totalSent) * 100 : 0;

    return {
      totalSent,
      openRate: Math.round(openRate),
      clickRate: Math.round(clickRate),
      unsubscribeRate: 0, // TODO: Track unsubscribes
      bounceRate: Math.round(bounceRate),
      byTemplate: new Map(), // TODO: Aggregate by template
    };
  }

  /**
   * Calculate training metrics
   */
  private calculateTrainingMetrics(): TrainingMetrics {
    const trainingEvents = this.metrics.filter((m) => m.category === 'feature_usage');

    return {
      startedCount: this.userMetrics.size,
      completedCount: Array.from(this.userMetrics.values()).filter(
        (m) => m.trainingCompletionRate === 100
      ).length,
      completionRate: Math.round(
        Array.from(this.userMetrics.values()).reduce((sum, m) => sum + m.trainingCompletionRate, 0) /
          (this.userMetrics.size || 1)
      ),
      averageTimePerMaterial: 0, // TODO: Calculate from actual training data
      mostAccessedMaterials: [], // TODO: Track material access
      difficulty_distribution: {
        beginner: 0,
        intermediate: 0,
        advanced: 0,
      },
    };
  }

  /**
   * Get metrics for a time range
   */
  getMetricsForTimeRange(startDate: Date, endDate: Date): SuccessMetric[] {
    return this.metrics.filter((m) => m.timestamp >= startDate && m.timestamp <= endDate);
  }

  /**
   * Get metric trend
   */
  getMetricTrend(
    metricName: string,
    category: string,
    days: number = 30
  ): { date: Date; value: number }[] {
    const key = `${category}:${metricName}`;
    const timeseries = this.metricTimeseries.get(key) || [];

    const endDate = new Date();
    const startDate = new Date(endDate.getTime() - days * 24 * 60 * 60 * 1000);

    const filtered = timeseries.filter((p) => p.timestamp >= startDate && p.timestamp <= endDate);

    // Group by day and aggregate
    const dailyAggregates = new Map<string, number[]>();
    filtered.forEach((point) => {
      const dateKey = point.timestamp.toISOString().split('T')[0];
      const values = dailyAggregates.get(dateKey) || [];
      values.push(point.value);
      dailyAggregates.set(dateKey, values);
    });

    return Array.from(dailyAggregates.entries())
      .map(([dateStr, values]) => ({
        date: new Date(dateStr),
        value: values.reduce((a, b) => a + b, 0) / values.length,
      }))
      .sort((a, b) => a.date.getTime() - b.date.getTime());
  }

  /**
   * Clear metrics (for testing)
   */
  clearMetrics(): void {
    this.metrics = [];
    this.userMetrics.clear();
    this.metricTimeseries.clear();
    this.onboardingStates.clear();
  }

  /**
   * Export metrics as JSON
   */
  exportMetrics(): unknown {
    return {
      metrics: this.metrics,
      userMetrics: Array.from(this.userMetrics.entries()),
      analytics: this.getAnalytics(),
      timestamp: new Date(),
    };
  }
}
