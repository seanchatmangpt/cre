/**
 * Configuration Manager Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { ConfigurationManager } from '@/onboarding/ConfigurationManager';
import {
  User,
  SetupConfiguration,
  OnboardingStep,
  OnboardingState,
} from '@/onboarding/types';

class MockStorageBackend {
  private storage = new Map<string, unknown>();

  async save(key: string, data: unknown): Promise<void> {
    this.storage.set(key, data);
  }

  async load(key: string): Promise<unknown> {
    return this.storage.get(key);
  }

  async delete(key: string): Promise<void> {
    this.storage.delete(key);
  }

  clear(): void {
    this.storage.clear();
  }
}

describe('ConfigurationManager', () => {
  let manager: ConfigurationManager;
  let storage: MockStorageBackend;

  const mockUser: User = {
    id: 'user-123',
    email: 'user@example.com',
    name: 'John Doe',
    organizationId: 'org-123',
    role: 'user',
    createdAt: new Date(),
  };

  beforeEach(() => {
    storage = new MockStorageBackend();
    manager = new ConfigurationManager(storage);
  });

  describe('configuration initialization', () => {
    it('should initialize configuration for a new user', async () => {
      const config = await manager.initializeConfiguration(mockUser);

      expect(config).toBeDefined();
      expect(config.timezone).toBe('UTC');
      expect(config.language).toBe('en');
    });

    it('should return cached configuration on subsequent calls', async () => {
      await manager.initializeConfiguration(mockUser);
      const config2 = await manager.initializeConfiguration(mockUser);

      expect(config2).toBeDefined();
    });

    it('should persist configuration to storage', async () => {
      await manager.initializeConfiguration(mockUser);

      const stored = await storage.load(`config:${mockUser.id}`);
      expect(stored).toBeDefined();
    });

    it('should apply default values', async () => {
      const config = await manager.initializeConfiguration(mockUser);

      expect(config.organizationSize).toBe(50);
      expect(config.integrationType).toBe('none');
    });
  });

  describe('configuration updates', () => {
    it('should update configuration', async () => {
      await manager.initializeConfiguration(mockUser);

      const updates: Partial<SetupConfiguration> = {
        organizationName: 'Test Org',
        organizationSize: 100,
      };

      const updated = await manager.updateConfiguration(mockUser.id, updates);

      expect(updated.organizationName).toBe('Test Org');
      expect(updated.organizationSize).toBe(100);
    });

    it('should persist updates to storage', async () => {
      await manager.initializeConfiguration(mockUser);
      await manager.updateConfiguration(mockUser.id, { organizationName: 'Test Org' });

      const stored = await storage.load(`config:${mockUser.id}`);
      expect((stored as any).organizationName).toBe('Test Org');
    });

    it('should throw error when updating non-existent configuration', async () => {
      await expect(
        manager.updateConfiguration('non-existent-user', { organizationName: 'Test' })
      ).rejects.toThrow();
    });
  });

  describe('configuration validation', () => {
    it('should validate required fields', () => {
      const config: SetupConfiguration = {
        organizationSize: 0,
      };

      const result = manager.validateConfiguration(config);

      expect(result.isValid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });

    it('should validate organization size', () => {
      const config: SetupConfiguration = {
        organizationName: 'Test Org',
        organizationSize: 0,
        timezone: 'UTC',
        language: 'en',
      };

      const result = manager.validateConfiguration(config);

      expect(result.isValid).toBe(false);
      expect(result.errors.some((e) => e.includes('size'))).toBe(true);
    });

    it('should validate integration type', () => {
      const config: SetupConfiguration = {
        organizationName: 'Test Org',
        organizationSize: 50,
        timezone: 'UTC',
        language: 'en',
        integrationType: 'invalid' as any,
      };

      const result = manager.validateConfiguration(config);

      expect(result.isValid).toBe(false);
    });

    it('should accept valid configuration', () => {
      const config: SetupConfiguration = {
        organizationName: 'Test Org',
        organizationSize: 50,
        timezone: 'UTC',
        language: 'en',
        integrationType: 'api',
      };

      const result = manager.validateConfiguration(config);

      expect(result.isValid).toBe(true);
      expect(result.errors.length).toBe(0);
    });
  });

  describe('onboarding state', () => {
    it('should initialize onboarding state for a user', async () => {
      const state = await manager.initializeOnboardingState(mockUser);

      expect(state).toBeDefined();
      expect(state.userId).toBe(mockUser.id);
      expect(state.currentStep).toBe(OnboardingStep.WELCOME);
      expect(state.completedSteps.length).toBe(0);
    });

    it('should retrieve onboarding state', async () => {
      await manager.initializeOnboardingState(mockUser);

      const state = await manager.getOnboardingState(mockUser.id);

      expect(state).toBeDefined();
      expect(state?.userId).toBe(mockUser.id);
    });

    it('should mark steps as completed', async () => {
      await manager.initializeOnboardingState(mockUser);

      const updated = await manager.markStepCompleted(mockUser.id, OnboardingStep.WELCOME);

      expect(updated.completedSteps).toContain(OnboardingStep.WELCOME);
      expect(updated.currentStep).toBe(OnboardingStep.BASIC_SETUP);
    });

    it('should not duplicate completed steps', async () => {
      await manager.initializeOnboardingState(mockUser);

      await manager.markStepCompleted(mockUser.id, OnboardingStep.WELCOME);
      const updated = await manager.markStepCompleted(mockUser.id, OnboardingStep.WELCOME);

      const welcomeCount = updated.completedSteps.filter(
        (s) => s === OnboardingStep.WELCOME
      ).length;
      expect(welcomeCount).toBe(1);
    });

    it('should complete onboarding', async () => {
      await manager.initializeOnboardingState(mockUser);

      const completed = await manager.completeOnboarding(mockUser.id);

      expect(completed.completedAt).toBeDefined();
      expect(completed.currentStep).toBe(OnboardingStep.COMPLETION);
    });

    it('should mark onboarding as abandoned', async () => {
      await manager.initializeOnboardingState(mockUser);

      const abandoned = await manager.abandonOnboarding(mockUser.id);

      expect(abandoned.abandonedAt).toBeDefined();
    });
  });

  describe('defaults management', () => {
    it('should get default values', () => {
      const defaults = manager.getDefaults();

      expect(defaults.timezone).toBe('UTC');
      expect(defaults.language).toBe('en');
      expect(defaults.organizationSize).toBe(50);
    });

    it('should update default values', () => {
      manager.updateDefaults({ timezone: 'EST', organizationSize: 100 });

      const defaults = manager.getDefaults();

      expect(defaults.timezone).toBe('EST');
      expect(defaults.organizationSize).toBe(100);
    });

    it('should apply updated defaults to new configurations', async () => {
      manager.updateDefaults({ timezone: 'PST', organizationSize: 200 });

      const config = await manager.initializeConfiguration(mockUser);

      expect(config.timezone).toBe('PST');
      expect(config.organizationSize).toBe(200);
    });
  });

  describe('analytics', () => {
    it('should retrieve all configurations', async () => {
      const user2: User = { ...mockUser, id: 'user-456' };

      await manager.initializeConfiguration(mockUser);
      await manager.initializeConfiguration(user2);

      const configs = manager.getAllConfigurations();

      expect(configs.size).toBe(2);
    });

    it('should retrieve all onboarding states', async () => {
      const user2: User = { ...mockUser, id: 'user-456' };

      await manager.initializeOnboardingState(mockUser);
      await manager.initializeOnboardingState(user2);

      const states = manager.getAllOnboardingStates();

      expect(states.size).toBe(2);
    });
  });

  describe('caching', () => {
    it('should load configuration from storage if not cached', async () => {
      await manager.initializeConfiguration(mockUser);

      // Create new manager instance to bypass cache
      const newManager = new ConfigurationManager(storage);
      const loaded = await newManager.getConfiguration(mockUser.id);

      expect(loaded).toBeDefined();
      expect(loaded?.organizationSize).toBe(50);
    });

    it('should clear cache', async () => {
      await manager.initializeConfiguration(mockUser);
      manager.clearCache();

      const configs = manager.getAllConfigurations();
      expect(configs.size).toBe(0);
    });
  });
});
