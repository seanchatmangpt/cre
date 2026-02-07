/**
 * Metrics Tracker Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { MetricsTracker } from '@/onboarding/MetricsTracker';
import {
  User,
  OnboardingStep,
  SuccessMetric,
  OnboardingState,
} from '@/onboarding/types';

describe('MetricsTracker', () => {
  let tracker: MetricsTracker;

  const mockUser: User = {
    id: 'user-123',
    email: 'user@example.com',
    name: 'John Doe',
    organizationId: 'org-123',
    role: 'user',
    createdAt: new Date(),
  };

  const mockOnboardingState: OnboardingState = {
    userId: mockUser.id,
    completedSteps: [OnboardingStep.WELCOME, OnboardingStep.BASIC_SETUP],
    currentStep: OnboardingStep.PROFILE_CONFIGURATION,
    configuration: {
      organizationName: 'Test Org',
      organizationSize: 50,
      timezone: 'UTC',
      language: 'en',
    },
    startedAt: new Date(Date.now() - 60 * 60 * 1000), // 1 hour ago
  };

  beforeEach(() => {
    tracker = new MetricsTracker();
  });

  describe('metric recording', () => {
    it('should record a success metric', () => {
      const metric: SuccessMetric = {
        id: 'metric-1',
        metricName: 'test_metric',
        value: 100,
        unit: 'count',
        category: 'engagement',
        timestamp: new Date(),
        userId: mockUser.id,
      };

      tracker.recordMetric(metric);

      const exported = tracker.exportMetrics() as any;
      expect(exported.metrics.length).toBe(1);
    });

    it('should record email events', () => {
      tracker.recordEmailEvent(mockUser.id, 'template-1', 'open');

      const exported = tracker.exportMetrics() as any;
      expect(exported.metrics.length).toBe(1);
      expect(exported.metrics[0].metricName).toBe('email_open');
    });

    it('should record step completions', () => {
      tracker.recordStepCompletion(mockUser.id, OnboardingStep.WELCOME);

      const exported = tracker.exportMetrics() as any;
      expect(exported.metrics.some((m: any) => m.metricName === 'step_completed')).toBe(true);
    });

    it('should record feature usage', () => {
      tracker.recordFeatureUsage(mockUser.id, 'dashboard', 5);

      const exported = tracker.exportMetrics() as any;
      expect(exported.metrics.some((m: any) => m.metricName === 'feature_usage')).toBe(true);
    });
  });

  describe('user metrics initialization', () => {
    it('should initialize user metrics', () => {
      const metrics = tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      expect(metrics).toBeDefined();
      expect(metrics.userId).toBe(mockUser.id);
      expect(metrics.organizationId).toBe(mockUser.organizationId);
      expect(metrics.overallHealthScore).toBe(0);
    });

    it('should retrieve initialized metrics', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      const metrics = tracker.getUserMetrics(mockUser.id);
      expect(metrics).toBeDefined();
      expect(metrics?.userId).toBe(mockUser.id);
    });
  });

  describe('user metrics updates', () => {
    it('should update user metrics based on onboarding state', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      const updated = tracker.updateUserMetrics(mockUser.id, mockOnboardingState);

      expect(updated).toBeDefined();
      expect(updated.stepCompletionRates.size).toBeGreaterThan(0);
    });

    it('should track completion date', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      const completedState: OnboardingState = {
        ...mockOnboardingState,
        completedAt: new Date(),
      };

      const updated = tracker.updateUserMetrics(mockUser.id, completedState);

      expect(updated.completionDate).toBeDefined();
      expect(updated.timeToComplete).toBeDefined();
    });

    it('should calculate health score', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      tracker.updateUserMetrics(mockUser.id, mockOnboardingState);

      const metrics = tracker.getUserMetrics(mockUser.id);
      expect(metrics?.overallHealthScore).toBeGreaterThanOrEqual(0);
      expect(metrics?.overallHealthScore).toBeLessThanOrEqual(100);
    });

    it('should generate recommendations', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      tracker.updateUserMetrics(mockUser.id, mockOnboardingState);

      const metrics = tracker.getUserMetrics(mockUser.id);
      expect(Array.isArray(metrics?.recommendations)).toBe(true);
    });
  });

  describe('analytics', () => {
    it('should calculate analytics dashboard data', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);
      tracker.updateUserMetrics(mockUser.id, mockOnboardingState);

      const analytics = tracker.getAnalytics();

      expect(analytics).toBeDefined();
      expect(analytics.totalUsersStarted).toBe(1);
      expect(analytics.completionRate).toBeGreaterThanOrEqual(0);
    });

    it('should calculate completion rate', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      const completedState: OnboardingState = {
        ...mockOnboardingState,
        completedAt: new Date(),
      };

      tracker.updateUserMetrics(mockUser.id, completedState);

      const analytics = tracker.getAnalytics();
      expect(analytics.totalUsersCompleted).toBe(1);
    });

    it('should calculate step dropoff rates', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);
      tracker.updateUserMetrics(mockUser.id, mockOnboardingState);

      const analytics = tracker.getAnalytics();

      expect(analytics.stepDropoffRates).toBeDefined();
      expect(analytics.stepDropoffRates.size).toBeGreaterThan(0);
    });

    it('should calculate average time to completion', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      const completedState: OnboardingState = {
        ...mockOnboardingState,
        completedAt: new Date(),
      };

      tracker.updateUserMetrics(mockUser.id, completedState);

      const analytics = tracker.getAnalytics();
      expect(analytics.averageTimeToCompletion).toBeGreaterThan(0);
    });
  });

  describe('email metrics', () => {
    it('should track email engagement metrics', () => {
      tracker.recordEmailEvent(mockUser.id, 'template-1', 'open');
      tracker.recordEmailEvent(mockUser.id, 'template-1', 'click');

      const analytics = tracker.getAnalytics();

      expect(analytics.emailEngagementMetrics).toBeDefined();
    });
  });

  describe('training metrics', () => {
    it('should track training metrics', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);

      const analytics = tracker.getAnalytics();

      expect(analytics.trainingMetrics).toBeDefined();
      expect(analytics.trainingMetrics.startedCount).toBeGreaterThanOrEqual(0);
    });
  });

  describe('metric trends', () => {
    it('should calculate metric trends', () => {
      const now = new Date();

      for (let i = 0; i < 5; i++) {
        tracker.recordMetric({
          id: `metric-${i}`,
          metricName: 'test_metric',
          value: i * 10,
          unit: 'count',
          category: 'engagement',
          timestamp: new Date(now.getTime() - i * 24 * 60 * 60 * 1000),
        });
      }

      const trend = tracker.getMetricTrend('test_metric', 'engagement', 30);

      expect(trend.length).toBeGreaterThan(0);
    });

    it('should group metrics by day', () => {
      const now = new Date();

      // Add multiple metrics on the same day
      for (let i = 0; i < 3; i++) {
        tracker.recordMetric({
          id: `metric-${i}`,
          metricName: 'test_metric',
          value: 10,
          unit: 'count',
          category: 'engagement',
          timestamp: new Date(now.getTime() - i * 60 * 60 * 1000),
        });
      }

      const trend = tracker.getMetricTrend('test_metric', 'engagement', 1);

      expect(trend.length).toBeGreaterThan(0);
    });
  });

  describe('time range queries', () => {
    it('should retrieve metrics for a time range', () => {
      const now = new Date();
      const startDate = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
      const endDate = now;

      tracker.recordMetric({
        id: 'metric-1',
        metricName: 'test_metric',
        value: 100,
        unit: 'count',
        category: 'engagement',
        timestamp: new Date(now.getTime() - 3 * 24 * 60 * 60 * 1000),
      });

      const metrics = tracker.getMetricsForTimeRange(startDate, endDate);

      expect(metrics.length).toBe(1);
    });

    it('should exclude metrics outside time range', () => {
      const now = new Date();
      const startDate = new Date(now.getTime() - 2 * 24 * 60 * 60 * 1000);
      const endDate = new Date(now.getTime() - 1 * 24 * 60 * 60 * 1000);

      tracker.recordMetric({
        id: 'metric-1',
        metricName: 'test_metric',
        value: 100,
        unit: 'count',
        category: 'engagement',
        timestamp: new Date(now.getTime() - 5 * 24 * 60 * 60 * 1000),
      });

      const metrics = tracker.getMetricsForTimeRange(startDate, endDate);

      expect(metrics.length).toBe(0);
    });
  });

  describe('export', () => {
    it('should export metrics as JSON', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);
      tracker.recordMetric({
        id: 'metric-1',
        metricName: 'test_metric',
        value: 100,
        unit: 'count',
        category: 'engagement',
        timestamp: new Date(),
      });

      const exported = tracker.exportMetrics() as any;

      expect(exported).toBeDefined();
      expect(exported.metrics).toBeDefined();
      expect(exported.userMetrics).toBeDefined();
      expect(exported.analytics).toBeDefined();
    });
  });

  describe('cache management', () => {
    it('should clear metrics', () => {
      tracker.initializeUserMetrics(mockUser.id, mockUser.organizationId);
      tracker.recordMetric({
        id: 'metric-1',
        metricName: 'test_metric',
        value: 100,
        unit: 'count',
        category: 'engagement',
        timestamp: new Date(),
      });

      tracker.clearMetrics();

      const metrics = tracker.getUserMetrics(mockUser.id);
      expect(metrics).toBeUndefined();
    });
  });
});
