/**
 * Configuration Manager
 * Manages setup configuration state, persistence, and validation
 */

import { SetupConfiguration, User, OnboardingState, OnboardingStep } from './types';

interface StorageBackend {
  save(key: string, data: unknown): Promise<void>;
  load(key: string): Promise<unknown>;
  delete(key: string): Promise<void>;
}

interface ConfigurationDefaults {
  timezone: string;
  language: string;
  organizationSize: number;
}

export class ConfigurationManager {
  private configurations: Map<string, SetupConfiguration> = new Map();
  private onboardingStates: Map<string, OnboardingState> = new Map();
  private defaults: ConfigurationDefaults = {
    timezone: 'UTC',
    language: 'en',
    organizationSize: 50,
  };

  constructor(private storageBackend: StorageBackend) {}

  /**
   * Initialize configuration for a user
   */
  async initializeConfiguration(user: User): Promise<SetupConfiguration> {
    const existingConfig = this.configurations.get(user.id);
    if (existingConfig) {
      return existingConfig;
    }

    const config: SetupConfiguration = {
      organizationName: undefined,
      organizationSize: this.defaults.organizationSize,
      department: undefined,
      primaryUseCase: undefined,
      integrationType: 'none',
      customSettings: {},
      timezone: this.defaults.timezone,
      language: this.defaults.language,
    };

    this.configurations.set(user.id, config);
    await this.persistConfiguration(user.id, config);

    return config;
  }

  /**
   * Get configuration for a user
   */
  async getConfiguration(userId: string): Promise<SetupConfiguration | null> {
    // Check memory first
    const cached = this.configurations.get(userId);
    if (cached) {
      return cached;
    }

    // Load from storage
    try {
      const stored = await this.storageBackend.load(`config:${userId}`);
      if (stored && typeof stored === 'object') {
        const config = stored as SetupConfiguration;
        this.configurations.set(userId, config);
        return config;
      }
    } catch (error) {
      console.error(`Failed to load configuration for ${userId}:`, error);
    }

    return null;
  }

  /**
   * Update configuration for a user
   */
  async updateConfiguration(userId: string, updates: Partial<SetupConfiguration>): Promise<SetupConfiguration> {
    let config = this.configurations.get(userId);
    if (!config) {
      throw new Error(`Configuration not found for user ${userId}`);
    }

    config = { ...config, ...updates };
    this.configurations.set(userId, config);
    await this.persistConfiguration(userId, config);

    return config;
  }

  /**
   * Validate configuration
   */
  validateConfiguration(config: SetupConfiguration): { isValid: boolean; errors: string[] } {
    const errors: string[] = [];

    if (!config.organizationName || config.organizationName.trim().length === 0) {
      errors.push('Organization name is required');
    }

    if (!config.organizationSize || config.organizationSize <= 0) {
      errors.push('Organization size must be greater than 0');
    }

    if (!config.timezone) {
      errors.push('Timezone is required');
    }

    if (!config.language) {
      errors.push('Language is required');
    }

    if (config.integrationType && !['api', 'webhook', 'none'].includes(config.integrationType)) {
      errors.push('Invalid integration type');
    }

    return {
      isValid: errors.length === 0,
      errors,
    };
  }

  /**
   * Initialize onboarding state for a user
   */
  async initializeOnboardingState(user: User): Promise<OnboardingState> {
    const config = await this.initializeConfiguration(user);

    const state: OnboardingState = {
      userId: user.id,
      completedSteps: [],
      currentStep: OnboardingStep.WELCOME,
      configuration: config,
      startedAt: new Date(),
    };

    this.onboardingStates.set(user.id, state);
    await this.persistOnboardingState(user.id, state);

    return state;
  }

  /**
   * Get onboarding state for a user
   */
  async getOnboardingState(userId: string): Promise<OnboardingState | null> {
    // Check memory first
    const cached = this.onboardingStates.get(userId);
    if (cached) {
      return cached;
    }

    // Load from storage
    try {
      const stored = await this.storageBackend.load(`onboarding:${userId}`);
      if (stored && typeof stored === 'object') {
        const state = stored as OnboardingState;
        this.onboardingStates.set(userId, state);
        return state;
      }
    } catch (error) {
      console.error(`Failed to load onboarding state for ${userId}:`, error);
    }

    return null;
  }

  /**
   * Mark a step as completed
   */
  async markStepCompleted(userId: string, step: OnboardingStep): Promise<OnboardingState> {
    let state = this.onboardingStates.get(userId);
    if (!state) {
      throw new Error(`Onboarding state not found for user ${userId}`);
    }

    if (!state.completedSteps.includes(step)) {
      state.completedSteps.push(step);
    }

    // Move to next step
    const allSteps = Object.values(OnboardingStep);
    const currentIndex = allSteps.indexOf(state.currentStep);
    if (currentIndex < allSteps.length - 1) {
      state.currentStep = allSteps[currentIndex + 1] as OnboardingStep;
    }

    this.onboardingStates.set(userId, state);
    await this.persistOnboardingState(userId, state);

    return state;
  }

  /**
   * Complete onboarding for a user
   */
  async completeOnboarding(userId: string): Promise<OnboardingState> {
    let state = this.onboardingStates.get(userId);
    if (!state) {
      throw new Error(`Onboarding state not found for user ${userId}`);
    }

    state = {
      ...state,
      currentStep: OnboardingStep.COMPLETION,
      completedAt: new Date(),
      completedSteps: Object.values(OnboardingStep),
    };

    this.onboardingStates.set(userId, state);
    await this.persistOnboardingState(userId, state);

    return state;
  }

  /**
   * Abandon onboarding
   */
  async abandonOnboarding(userId: string): Promise<OnboardingState> {
    let state = this.onboardingStates.get(userId);
    if (!state) {
      throw new Error(`Onboarding state not found for user ${userId}`);
    }

    state = {
      ...state,
      abandonedAt: new Date(),
    };

    this.onboardingStates.set(userId, state);
    await this.persistOnboardingState(userId, state);

    return state;
  }

  /**
   * Persist configuration to storage
   */
  private async persistConfiguration(userId: string, config: SetupConfiguration): Promise<void> {
    try {
      await this.storageBackend.save(`config:${userId}`, config);
    } catch (error) {
      console.error(`Failed to persist configuration for ${userId}:`, error);
      throw new Error(`Failed to save configuration: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  /**
   * Persist onboarding state to storage
   */
  private async persistOnboardingState(userId: string, state: OnboardingState): Promise<void> {
    try {
      await this.storageBackend.save(`onboarding:${userId}`, state);
    } catch (error) {
      console.error(`Failed to persist onboarding state for ${userId}:`, error);
      throw new Error(`Failed to save onboarding state: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  /**
   * Get configuration defaults
   */
  getDefaults(): ConfigurationDefaults {
    return { ...this.defaults };
  }

  /**
   * Update configuration defaults
   */
  updateDefaults(defaults: Partial<ConfigurationDefaults>): void {
    this.defaults = { ...this.defaults, ...defaults };
  }

  /**
   * Clear all cached configurations (for testing)
   */
  clearCache(): void {
    this.configurations.clear();
    this.onboardingStates.clear();
  }

  /**
   * Get all configurations (for analytics)
   */
  getAllConfigurations(): Map<string, SetupConfiguration> {
    return new Map(this.configurations);
  }

  /**
   * Get all onboarding states (for analytics)
   */
  getAllOnboardingStates(): Map<string, OnboardingState> {
    return new Map(this.onboardingStates);
  }
}
