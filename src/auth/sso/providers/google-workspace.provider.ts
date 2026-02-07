/**
 * Google Workspace SSO Provider
 * Supports OAuth2 and OIDC protocols
 */

import { OIDCService } from '../oidc/oidc.service';
import { OIDCConfig, SSOProvider, AuthProtocol, SSOUser } from '../types';

export interface GoogleWorkspaceConfig {
  clientId: string;
  clientSecret: string;
  domain?: string; // Restrict to specific G Suite domain
  hostedDomain?: string; // hd parameter for domain restriction
  callbackUrl: string;
  scopes?: string[];
}

export class GoogleWorkspaceProvider {
  private oidcService: OIDCService;
  private config: GoogleWorkspaceConfig;

  private static readonly DISCOVERY_URL =
    'https://accounts.google.com/.well-known/openid-configuration';

  private static readonly DEFAULT_SCOPES = [
    'openid',
    'email',
    'profile',
    'https://www.googleapis.com/auth/userinfo.email',
    'https://www.googleapis.com/auth/userinfo.profile',
  ];

  constructor(config: GoogleWorkspaceConfig) {
    this.config = config;

    const oidcConfig: OIDCConfig = {
      provider: SSOProvider.GOOGLE_WORKSPACE,
      protocol: AuthProtocol.OIDC,
      enabled: true,
      clientId: config.clientId,
      clientSecret: config.clientSecret,
      callbackUrl: config.callbackUrl,
      discoveryUrl: GoogleWorkspaceProvider.DISCOVERY_URL,
      issuer: 'https://accounts.google.com',
      jwksUri: 'https://www.googleapis.com/oauth2/v3/certs',
      authorizationUrl: 'https://accounts.google.com/o/oauth2/v2/auth',
      tokenUrl: 'https://oauth2.googleapis.com/token',
      userinfoUrl: 'https://openidconnect.googleapis.com/v1/userinfo',
      scope: config.scopes || GoogleWorkspaceProvider.DEFAULT_SCOPES,
      pkce: true,
      metadata: {
        revokeUrl: 'https://oauth2.googleapis.com/revoke',
        domain: config.domain,
        hostedDomain: config.hostedDomain,
      },
    };

    this.oidcService = new OIDCService(oidcConfig);
  }

  /**
   * Generate Google OAuth2 authorization URL
   */
  getAuthorizationUrl(options?: {
    accessType?: 'online' | 'offline';
    prompt?: 'none' | 'consent' | 'select_account';
  }): { url: string; state: string } {
    const { url, state } = this.oidcService.generateAuthorizationUrl();

    // Add Google-specific parameters
    const urlObj = new URL(url);
    if (options?.accessType) {
      urlObj.searchParams.set('access_type', options.accessType);
    }
    if (options?.prompt) {
      urlObj.searchParams.set('prompt', options.prompt);
    }
    if (this.config.hostedDomain) {
      urlObj.searchParams.set('hd', this.config.hostedDomain);
    }

    return {
      url: urlObj.toString(),
      state,
    };
  }

  /**
   * Handle OAuth2 callback and authenticate user
   */
  async handleCallback(
    code: string,
    state: string
  ): Promise<{
    user: SSOUser;
    accessToken: string;
    refreshToken?: string;
    idToken: string;
  }> {
    // Exchange code for tokens
    const tokens = await this.oidcService.exchangeCodeForTokens(code, state);

    // Validate ID token
    const validation = await this.oidcService.validateIdToken(tokens.idToken!);
    if (!validation.valid || !validation.user) {
      throw new Error(`ID token validation failed: ${validation.error}`);
    }

    // Validate hosted domain if configured
    if (this.config.hostedDomain) {
      const hd = (validation.user.rawProfile as any)?.hd;
      if (hd !== this.config.hostedDomain) {
        throw new Error(
          `User domain ${hd} does not match required domain ${this.config.hostedDomain}`
        );
      }
    }

    return {
      user: validation.user,
      accessToken: tokens.accessToken,
      refreshToken: tokens.refreshToken,
      idToken: tokens.idToken!,
    };
  }

  /**
   * Get user profile information
   */
  async getUserProfile(accessToken: string): Promise<SSOUser> {
    return this.oidcService.getUserInfo(accessToken);
  }

  /**
   * Refresh access token
   */
  async refreshToken(refreshToken: string): Promise<{
    accessToken: string;
    refreshToken?: string;
    expiresIn: number;
  }> {
    const tokens = await this.oidcService.refreshAccessToken(refreshToken);
    return {
      accessToken: tokens.accessToken,
      refreshToken: tokens.refreshToken,
      expiresIn: tokens.expiresIn,
    };
  }

  /**
   * Revoke access token
   */
  async revokeToken(token: string): Promise<boolean> {
    return this.oidcService.revokeToken(token);
  }

  /**
   * Get logout URL
   */
  getLogoutUrl(idToken: string, redirectUri?: string): Promise<string> {
    return this.oidcService.logout(idToken, redirectUri);
  }

  /**
   * Validate Google Workspace domain membership
   */
  async validateDomainMembership(email: string): Promise<boolean> {
    if (!this.config.domain) {
      return true;
    }

    const emailDomain = email.split('@')[1];
    return emailDomain === this.config.domain;
  }
}
