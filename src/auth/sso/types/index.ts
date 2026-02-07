/**
 * Enterprise SSO Type Definitions
 * Supports SAML 2.0, OAuth2, and OIDC protocols
 */

export enum SSOProvider {
  GOOGLE_WORKSPACE = 'google-workspace',
  OKTA = 'okta',
  AZURE_AD = 'azure-ad',
}

export enum AuthProtocol {
  SAML = 'saml',
  OAUTH2 = 'oauth2',
  OIDC = 'oidc',
}

export interface SSOConfig {
  provider: SSOProvider;
  protocol: AuthProtocol;
  enabled: boolean;
  clientId: string;
  clientSecret?: string;
  callbackUrl: string;
  logoutUrl?: string;
  metadata?: Record<string, unknown>;
}

export interface SAMLConfig extends SSOConfig {
  protocol: AuthProtocol.SAML;
  entryPoint: string;
  issuer: string;
  cert: string;
  privateCert?: string;
  signatureAlgorithm?: string;
  identifierFormat?: string;
  wantAssertionsSigned?: boolean;
  wantAuthnResponseSigned?: boolean;
}

export interface OAuth2Config extends SSOConfig {
  protocol: AuthProtocol.OAUTH2;
  authorizationUrl: string;
  tokenUrl: string;
  scope: string[];
  state?: string;
  pkce?: boolean;
}

export interface OIDCConfig extends OAuth2Config {
  protocol: AuthProtocol.OIDC;
  discoveryUrl: string;
  issuer: string;
  jwksUri: string;
  userinfoUrl?: string;
}

export interface SSOUser {
  id: string;
  email: string;
  name?: string;
  firstName?: string;
  lastName?: string;
  provider: SSOProvider;
  protocol: AuthProtocol;
  roles?: string[];
  groups?: string[];
  attributes?: Record<string, unknown>;
  rawProfile?: unknown;
}

export interface SSOSession {
  sessionId: string;
  userId: string;
  provider: SSOProvider;
  protocol: AuthProtocol;
  accessToken?: string;
  refreshToken?: string;
  idToken?: string;
  expiresAt: Date;
  createdAt: Date;
  lastAccessedAt: Date;
}

export interface TokenValidationResult {
  valid: boolean;
  user?: SSOUser;
  error?: string;
  expiresAt?: Date;
}

export interface SSOError {
  code: string;
  message: string;
  provider: SSOProvider;
  protocol: AuthProtocol;
  details?: unknown;
}

export interface ProviderMetadata {
  name: string;
  provider: SSOProvider;
  supportedProtocols: AuthProtocol[];
  discoveryUrl?: string;
  documentation?: string;
}

export interface SSOCallbackData {
  code?: string;
  state?: string;
  samlResponse?: string;
  idToken?: string;
  accessToken?: string;
  error?: string;
  errorDescription?: string;
}
