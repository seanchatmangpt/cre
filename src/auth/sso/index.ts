/**
 * Enterprise SSO Module
 * Main entry point for SSO functionality
 */

// Types
export * from './types';

// Services
export { SAMLService } from './saml/saml.service';
export { OAuth2Service } from './oauth/oauth.service';
export { OIDCService } from './oidc/oidc.service';

// Providers
export { GoogleWorkspaceProvider, GoogleWorkspaceConfig } from './providers/google-workspace.provider';
export { OktaProvider, OktaConfig } from './providers/okta.provider';
export { AzureADProvider, AzureADConfig } from './providers/azure-ad.provider';

// Configuration
export { ProviderRegistry, providerRegistry } from './config/provider-registry';
export { SSOConfigManager, ssoConfig } from './config/sso.config';

// Session Management
export { SessionManager, InMemorySessionStore } from './utils/session-manager';
export type { SessionStore } from './utils/session-manager';

// Middleware
export {
  createSSOMiddleware,
  optionalSSOAuth,
  requireSSOAuth,
  requireRole,
  requireGroup,
  requireProvider,
} from './middleware/sso-auth.middleware';

// Controller
export { SSOController } from './controllers/sso.controller';

// Convenience function to initialize SSO
import { ssoConfig } from './config/sso.config';
import { providerRegistry } from './config/provider-registry';
import { SessionManager, InMemorySessionStore } from './utils/session-manager';
import { SSOController } from './controllers/sso.controller';
import { Router } from 'express';

export interface InitializeSSOOptions {
  sessionStore?: any; // Use Redis, Database, etc. in production
  sessionMaxAge?: number;
  sessionCookieName?: string;
  sessionSecure?: boolean;
  csrfProtection?: boolean;
}

/**
 * Initialize SSO with configuration from environment variables
 */
export function initializeSSO(options: InitializeSSOOptions = {}): {
  router: Router;
  sessionManager: SessionManager;
  providerRegistry: ProviderRegistry;
  config: SSOConfigManager;
} {
  // Validate configuration
  const validation = ssoConfig.validate();
  if (!validation.valid) {
    throw new Error(
      `SSO configuration is invalid:\n${validation.errors.join('\n')}`
    );
  }

  // Create session store
  const sessionStore = options.sessionStore || new InMemorySessionStore();

  // Create session manager
  const sessionManager = new SessionManager(
    sessionStore,
    options.sessionMaxAge || ssoConfig.getConfig().sessionMaxAge
  );

  // Create controller
  const controller = new SSOController({
    providerRegistry: ssoConfig.getRegistry(),
    sessionManager,
    sessionCookieName: options.sessionCookieName || ssoConfig.getConfig().sessionCookieName,
    sessionSecure: options.sessionSecure ?? ssoConfig.getConfig().sessionSecure,
    csrfProtection: options.csrfProtection ?? ssoConfig.getConfig().csrfProtection,
  });

  // Create router
  const router = controller.createRouter();

  return {
    router,
    sessionManager,
    providerRegistry: ssoConfig.getRegistry(),
    config: ssoConfig,
  };
}
