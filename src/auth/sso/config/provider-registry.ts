/**
 * SSO Provider Registry
 * Factory pattern for dynamic provider instantiation
 */

import { GoogleWorkspaceProvider, GoogleWorkspaceConfig } from '../providers/google-workspace.provider';
import { OktaProvider, OktaConfig } from '../providers/okta.provider';
import { AzureADProvider, AzureADConfig } from '../providers/azure-ad.provider';
import { SSOProvider, AuthProtocol } from '../types';

export type ProviderConfig =
  | GoogleWorkspaceConfig
  | OktaConfig
  | AzureADConfig;

export type ProviderInstance =
  | GoogleWorkspaceProvider
  | OktaProvider
  | AzureADProvider;

export interface ProviderRegistration {
  provider: SSOProvider;
  protocol: AuthProtocol;
  enabled: boolean;
  config: ProviderConfig;
  instance?: ProviderInstance;
}

export class ProviderRegistry {
  private providers: Map<string, ProviderRegistration> = new Map();

  /**
   * Register an SSO provider
   */
  register(registration: ProviderRegistration): void {
    const key = this.getProviderKey(registration.provider, registration.protocol);
    this.providers.set(key, registration);
  }

  /**
   * Get provider instance (lazy initialization)
   */
  getProvider(
    provider: SSOProvider,
    protocol?: AuthProtocol
  ): ProviderInstance {
    // Find provider, optionally matching protocol
    let registration: ProviderRegistration | undefined;

    if (protocol) {
      const key = this.getProviderKey(provider, protocol);
      registration = this.providers.get(key);
    } else {
      // Find first enabled provider with matching name
      for (const reg of this.providers.values()) {
        if (reg.provider === provider && reg.enabled) {
          registration = reg;
          break;
        }
      }
    }

    if (!registration) {
      throw new Error(
        `Provider not found: ${provider}${protocol ? ` with protocol ${protocol}` : ''}`
      );
    }

    if (!registration.enabled) {
      throw new Error(`Provider ${provider} is disabled`);
    }

    // Lazy instantiation
    if (!registration.instance) {
      registration.instance = this.createProviderInstance(registration);
    }

    return registration.instance;
  }

  /**
   * Check if provider is registered
   */
  hasProvider(provider: SSOProvider, protocol?: AuthProtocol): boolean {
    if (protocol) {
      const key = this.getProviderKey(provider, protocol);
      return this.providers.has(key);
    }

    for (const reg of this.providers.values()) {
      if (reg.provider === provider) {
        return true;
      }
    }

    return false;
  }

  /**
   * Get all registered providers
   */
  getAllProviders(): ProviderRegistration[] {
    return Array.from(this.providers.values());
  }

  /**
   * Get enabled providers
   */
  getEnabledProviders(): ProviderRegistration[] {
    return Array.from(this.providers.values()).filter((reg) => reg.enabled);
  }

  /**
   * Unregister provider
   */
  unregister(provider: SSOProvider, protocol?: AuthProtocol): void {
    if (protocol) {
      const key = this.getProviderKey(provider, protocol);
      this.providers.delete(key);
    } else {
      // Remove all registrations for this provider
      const keysToDelete: string[] = [];
      for (const [key, reg] of this.providers.entries()) {
        if (reg.provider === provider) {
          keysToDelete.push(key);
        }
      }
      keysToDelete.forEach((key) => this.providers.delete(key));
    }
  }

  /**
   * Clear all providers
   */
  clear(): void {
    this.providers.clear();
  }

  private createProviderInstance(
    registration: ProviderRegistration
  ): ProviderInstance {
    switch (registration.provider) {
      case SSOProvider.GOOGLE_WORKSPACE:
        return new GoogleWorkspaceProvider(
          registration.config as GoogleWorkspaceConfig
        );

      case SSOProvider.OKTA:
        return new OktaProvider(registration.config as OktaConfig);

      case SSOProvider.AZURE_AD:
        return new AzureADProvider(registration.config as AzureADConfig);

      default:
        throw new Error(`Unknown provider: ${registration.provider}`);
    }
  }

  private getProviderKey(provider: SSOProvider, protocol: AuthProtocol): string {
    return `${provider}:${protocol}`;
  }
}

// Singleton instance
export const providerRegistry = new ProviderRegistry();
