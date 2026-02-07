/**
 * SSO Configuration Manager
 * Loads and validates configuration from environment variables
 */

import { SSOProvider, AuthProtocol } from '../types';
import { ProviderRegistry, providerRegistry } from './provider-registry';
import { GoogleWorkspaceConfig } from '../providers/google-workspace.provider';
import { OktaConfig } from '../providers/okta.provider';
import { AzureADConfig } from '../providers/azure-ad.provider';

export interface SSOConfiguration {
  // Session settings
  sessionSecret: string;
  sessionMaxAge: number; // milliseconds
  sessionCookieName: string;
  sessionSecure: boolean;

  // Security settings
  csrfProtection: boolean;
  stateExpiration: number; // milliseconds
  allowedOrigins: string[];

  // Providers
  providers: {
    googleWorkspace?: GoogleWorkspaceConfig & { enabled: boolean };
    okta?: OktaConfig & { enabled: boolean };
    azureAD?: AzureADConfig & { enabled: boolean };
  };
}

export class SSOConfigManager {
  private config: SSOConfiguration;
  private registry: ProviderRegistry;

  constructor() {
    this.config = this.loadConfiguration();
    this.registry = providerRegistry;
    this.initializeProviders();
  }

  /**
   * Load configuration from environment variables
   */
  private loadConfiguration(): SSOConfiguration {
    return {
      sessionSecret: this.requireEnv('SSO_SESSION_SECRET'),
      sessionMaxAge: parseInt(process.env.SSO_SESSION_MAX_AGE || '3600000', 10),
      sessionCookieName: process.env.SSO_SESSION_COOKIE_NAME || 'sso_session',
      sessionSecure: process.env.SSO_SESSION_SECURE === 'true',
      csrfProtection: process.env.SSO_CSRF_PROTECTION !== 'false',
      stateExpiration: parseInt(process.env.SSO_STATE_EXPIRATION || '600000', 10),
      allowedOrigins: (process.env.SSO_ALLOWED_ORIGINS || '').split(',').filter(Boolean),

      providers: {
        googleWorkspace: this.loadGoogleWorkspaceConfig(),
        okta: this.loadOktaConfig(),
        azureAD: this.loadAzureADConfig(),
      },
    };
  }

  /**
   * Load Google Workspace configuration
   */
  private loadGoogleWorkspaceConfig(): (GoogleWorkspaceConfig & { enabled: boolean }) | undefined {
    const enabled = process.env.SSO_GOOGLE_ENABLED === 'true';
    if (!enabled) return undefined;

    return {
      enabled: true,
      clientId: this.requireEnv('SSO_GOOGLE_CLIENT_ID'),
      clientSecret: this.requireEnv('SSO_GOOGLE_CLIENT_SECRET'),
      callbackUrl: this.requireEnv('SSO_GOOGLE_CALLBACK_URL'),
      domain: process.env.SSO_GOOGLE_DOMAIN,
      hostedDomain: process.env.SSO_GOOGLE_HOSTED_DOMAIN,
      scopes: (process.env.SSO_GOOGLE_SCOPES || '').split(',').filter(Boolean),
    };
  }

  /**
   * Load Okta configuration
   */
  private loadOktaConfig(): (OktaConfig & { enabled: boolean }) | undefined {
    const enabled = process.env.SSO_OKTA_ENABLED === 'true';
    if (!enabled) return undefined;

    const protocol = (process.env.SSO_OKTA_PROTOCOL || 'oidc') as 'oidc' | 'saml';

    const baseConfig = {
      enabled: true,
      domain: this.requireEnv('SSO_OKTA_DOMAIN'),
      clientId: this.requireEnv('SSO_OKTA_CLIENT_ID'),
      callbackUrl: this.requireEnv('SSO_OKTA_CALLBACK_URL'),
      protocol: protocol === 'oidc' ? AuthProtocol.OIDC : AuthProtocol.SAML,
    };

    if (protocol === 'oidc') {
      return {
        ...baseConfig,
        clientSecret: process.env.SSO_OKTA_CLIENT_SECRET,
        scopes: (process.env.SSO_OKTA_SCOPES || '').split(',').filter(Boolean),
      };
    } else {
      return {
        ...baseConfig,
        issuer: this.requireEnv('SSO_OKTA_ISSUER'),
        cert: this.requireEnv('SSO_OKTA_CERT'),
        entryPoint: this.requireEnv('SSO_OKTA_ENTRY_POINT'),
      };
    }
  }

  /**
   * Load Azure AD configuration
   */
  private loadAzureADConfig(): (AzureADConfig & { enabled: boolean }) | undefined {
    const enabled = process.env.SSO_AZURE_ENABLED === 'true';
    if (!enabled) return undefined;

    const protocol = (process.env.SSO_AZURE_PROTOCOL || 'oidc') as 'oidc' | 'oauth2' | 'saml';

    const baseConfig = {
      enabled: true,
      tenantId: this.requireEnv('SSO_AZURE_TENANT_ID'),
      clientId: this.requireEnv('SSO_AZURE_CLIENT_ID'),
      callbackUrl: this.requireEnv('SSO_AZURE_CALLBACK_URL'),
      protocol: protocol === 'oidc' ? AuthProtocol.OIDC :
                protocol === 'oauth2' ? AuthProtocol.OAUTH2 : AuthProtocol.SAML,
    };

    if (protocol === 'oidc' || protocol === 'oauth2') {
      return {
        ...baseConfig,
        clientSecret: process.env.SSO_AZURE_CLIENT_SECRET,
        scopes: (process.env.SSO_AZURE_SCOPES || '').split(',').filter(Boolean),
        domainHint: process.env.SSO_AZURE_DOMAIN_HINT,
        loginHint: process.env.SSO_AZURE_LOGIN_HINT,
        prompt: process.env.SSO_AZURE_PROMPT as any,
      };
    } else {
      return {
        ...baseConfig,
        issuer: this.requireEnv('SSO_AZURE_ISSUER'),
        cert: this.requireEnv('SSO_AZURE_CERT'),
        entryPoint: this.requireEnv('SSO_AZURE_ENTRY_POINT'),
      };
    }
  }

  /**
   * Initialize providers in registry
   */
  private initializeProviders(): void {
    const { googleWorkspace, okta, azureAD } = this.config.providers;

    if (googleWorkspace?.enabled) {
      this.registry.register({
        provider: SSOProvider.GOOGLE_WORKSPACE,
        protocol: AuthProtocol.OIDC,
        enabled: true,
        config: googleWorkspace,
      });
    }

    if (okta?.enabled) {
      this.registry.register({
        provider: SSOProvider.OKTA,
        protocol: okta.protocol,
        enabled: true,
        config: okta,
      });
    }

    if (azureAD?.enabled) {
      this.registry.register({
        provider: SSOProvider.AZURE_AD,
        protocol: azureAD.protocol,
        enabled: true,
        config: azureAD,
      });
    }
  }

  /**
   * Get full configuration
   */
  getConfig(): SSOConfiguration {
    return this.config;
  }

  /**
   * Get provider registry
   */
  getRegistry(): ProviderRegistry {
    return this.registry;
  }

  /**
   * Validate configuration
   */
  validate(): { valid: boolean; errors: string[] } {
    const errors: string[] = [];

    // Check session secret
    if (this.config.sessionSecret.length < 32) {
      errors.push('Session secret must be at least 32 characters');
    }

    // Check at least one provider is enabled
    const enabledProviders = Object.values(this.config.providers).filter(
      (p) => p?.enabled
    );
    if (enabledProviders.length === 0) {
      errors.push('At least one SSO provider must be enabled');
    }

    // Validate callback URLs
    for (const provider of enabledProviders) {
      try {
        new URL(provider.callbackUrl);
      } catch {
        errors.push(`Invalid callback URL for provider: ${provider.callbackUrl}`);
      }
    }

    return {
      valid: errors.length === 0,
      errors,
    };
  }

  private requireEnv(key: string): string {
    const value = process.env[key];
    if (!value) {
      throw new Error(`Missing required environment variable: ${key}`);
    }
    return value;
  }
}

// Singleton instance
export const ssoConfig = new SSOConfigManager();
