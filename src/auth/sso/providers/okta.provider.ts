/**
 * Okta SSO Provider
 * Supports SAML, OAuth2, and OIDC protocols
 */

import { OIDCService } from '../oidc/oidc.service';
import { SAMLService } from '../saml/saml.service';
import {
  OIDCConfig,
  SAMLConfig,
  SSOProvider,
  AuthProtocol,
  SSOUser,
} from '../types';

export interface OktaConfig {
  domain: string; // e.g., dev-123456.okta.com
  clientId: string;
  clientSecret?: string;
  protocol: AuthProtocol.OIDC | AuthProtocol.SAML;
  callbackUrl: string;
  scopes?: string[];
  // SAML-specific
  issuer?: string;
  cert?: string;
  entryPoint?: string;
}

export class OktaProvider {
  private service: OIDCService | SAMLService;
  private config: OktaConfig;
  private protocol: AuthProtocol;

  private static readonly DEFAULT_SCOPES = [
    'openid',
    'email',
    'profile',
    'groups',
  ];

  constructor(config: OktaConfig) {
    this.config = config;
    this.protocol = config.protocol;

    if (config.protocol === AuthProtocol.OIDC) {
      this.service = this.createOIDCService();
    } else {
      this.service = this.createSAMLService();
    }
  }

  private createOIDCService(): OIDCService {
    const issuer = `https://${this.config.domain}`;
    const oidcConfig: OIDCConfig = {
      provider: SSOProvider.OKTA,
      protocol: AuthProtocol.OIDC,
      enabled: true,
      clientId: this.config.clientId,
      clientSecret: this.config.clientSecret,
      callbackUrl: this.config.callbackUrl,
      discoveryUrl: `${issuer}/.well-known/openid-configuration`,
      issuer,
      jwksUri: `${issuer}/oauth2/v1/keys`,
      authorizationUrl: `${issuer}/oauth2/v1/authorize`,
      tokenUrl: `${issuer}/oauth2/v1/token`,
      userinfoUrl: `${issuer}/oauth2/v1/userinfo`,
      scope: this.config.scopes || OktaProvider.DEFAULT_SCOPES,
      pkce: true,
      metadata: {
        revokeUrl: `${issuer}/oauth2/v1/revoke`,
        introspectUrl: `${issuer}/oauth2/v1/introspect`,
        logoutUrl: `${issuer}/oauth2/v1/logout`,
      },
    };

    return new OIDCService(oidcConfig);
  }

  private createSAMLService(): SAMLService {
    if (!this.config.entryPoint || !this.config.cert || !this.config.issuer) {
      throw new Error(
        'SAML configuration requires entryPoint, cert, and issuer'
      );
    }

    const samlConfig: SAMLConfig = {
      provider: SSOProvider.OKTA,
      protocol: AuthProtocol.SAML,
      enabled: true,
      clientId: this.config.clientId,
      callbackUrl: this.config.callbackUrl,
      entryPoint: this.config.entryPoint,
      issuer: this.config.issuer,
      cert: this.config.cert,
      identifierFormat: 'urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress',
      wantAssertionsSigned: true,
      wantAuthnResponseSigned: true,
      signatureAlgorithm: 'sha256',
    };

    return new SAMLService(samlConfig);
  }

  /**
   * Get authorization URL (OIDC) or SAML request
   */
  async getAuthorizationUrl(): Promise<{ url: string; state?: string; id?: string }> {
    if (this.protocol === AuthProtocol.OIDC) {
      const oidcService = this.service as OIDCService;
      return oidcService.generateAuthorizationUrl();
    } else {
      const samlService = this.service as SAMLService;
      return samlService.generateAuthRequest();
    }
  }

  /**
   * Handle authentication callback
   */
  async handleCallback(data: {
    code?: string;
    state?: string;
    samlResponse?: string;
  }): Promise<{
    user: SSOUser;
    accessToken?: string;
    refreshToken?: string;
    idToken?: string;
  }> {
    if (this.protocol === AuthProtocol.OIDC) {
      return this.handleOIDCCallback(data.code!, data.state!);
    } else {
      return this.handleSAMLCallback(data.samlResponse!);
    }
  }

  private async handleOIDCCallback(
    code: string,
    state: string
  ): Promise<{
    user: SSOUser;
    accessToken: string;
    refreshToken?: string;
    idToken: string;
  }> {
    const oidcService = this.service as OIDCService;

    // Exchange code for tokens
    const tokens = await oidcService.exchangeCodeForTokens(code, state);

    // Validate ID token
    const validation = await oidcService.validateIdToken(tokens.idToken!);
    if (!validation.valid || !validation.user) {
      throw new Error(`ID token validation failed: ${validation.error}`);
    }

    // Enhance user with groups from userinfo endpoint
    const userWithGroups = await oidcService.getUserInfo(tokens.accessToken);

    return {
      user: userWithGroups,
      accessToken: tokens.accessToken,
      refreshToken: tokens.refreshToken,
      idToken: tokens.idToken!,
    };
  }

  private async handleSAMLCallback(
    samlResponse: string
  ): Promise<{
    user: SSOUser;
  }> {
    const samlService = this.service as SAMLService;

    const validation = await samlService.validateResponse(samlResponse);
    if (!validation.valid || !validation.user) {
      throw new Error(`SAML validation failed: ${validation.error}`);
    }

    return {
      user: validation.user,
    };
  }

  /**
   * Get user profile
   */
  async getUserProfile(accessToken: string): Promise<SSOUser> {
    if (this.protocol === AuthProtocol.OIDC) {
      const oidcService = this.service as OIDCService;
      return oidcService.getUserInfo(accessToken);
    }
    throw new Error('User profile not available for SAML');
  }

  /**
   * Refresh access token (OIDC only)
   */
  async refreshToken(refreshToken: string): Promise<{
    accessToken: string;
    refreshToken?: string;
    expiresIn: number;
  }> {
    if (this.protocol === AuthProtocol.OIDC) {
      const oidcService = this.service as OIDCService;
      const tokens = await oidcService.refreshAccessToken(refreshToken);
      return {
        accessToken: tokens.accessToken,
        refreshToken: tokens.refreshToken,
        expiresIn: tokens.expiresIn,
      };
    }
    throw new Error('Token refresh not available for SAML');
  }

  /**
   * Get logout URL
   */
  async getLogoutUrl(
    token: string,
    redirectUri?: string
  ): Promise<string> {
    if (this.protocol === AuthProtocol.OIDC) {
      const oidcService = this.service as OIDCService;
      return oidcService.logout(token, redirectUri);
    } else {
      const samlService = this.service as SAMLService;
      return samlService.generateLogoutRequest(token);
    }
  }

  /**
   * Validate Okta group membership
   */
  async validateGroupMembership(
    user: SSOUser,
    requiredGroups: string[]
  ): Promise<boolean> {
    if (!user.groups || user.groups.length === 0) {
      return false;
    }

    return requiredGroups.some((group) => user.groups!.includes(group));
  }

  /**
   * Get SAML metadata (SAML only)
   */
  async getServiceProviderMetadata(): Promise<string> {
    if (this.protocol === AuthProtocol.SAML) {
      const samlService = this.service as SAMLService;
      return samlService.generateServiceProviderMetadata();
    }
    throw new Error('Metadata only available for SAML');
  }
}
