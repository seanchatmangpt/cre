/**
 * Azure Active Directory (Azure AD) / Microsoft Entra ID SSO Provider
 * Supports OAuth2, OIDC, and SAML protocols
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

export interface AzureADConfig {
  tenantId: string; // Azure AD tenant ID or 'common', 'organizations', 'consumers'
  clientId: string;
  clientSecret?: string;
  protocol: AuthProtocol.OIDC | AuthProtocol.OAUTH2 | AuthProtocol.SAML;
  callbackUrl: string;
  scopes?: string[];
  // SAML-specific
  issuer?: string;
  cert?: string;
  entryPoint?: string;
  // Azure-specific
  domainHint?: string; // Pre-fill domain in login
  loginHint?: string; // Pre-fill username
  prompt?: 'login' | 'consent' | 'select_account' | 'none';
}

export class AzureADProvider {
  private service: OIDCService | SAMLService;
  private config: AzureADConfig;
  private protocol: AuthProtocol;

  private static readonly DEFAULT_SCOPES = [
    'openid',
    'profile',
    'email',
    'User.Read',
  ];

  constructor(config: AzureADConfig) {
    this.config = config;
    this.protocol = config.protocol;

    if (config.protocol === AuthProtocol.OIDC || config.protocol === AuthProtocol.OAUTH2) {
      this.service = this.createOIDCService();
    } else {
      this.service = this.createSAMLService();
    }
  }

  private createOIDCService(): OIDCService {
    const authority = `https://login.microsoftonline.com/${this.config.tenantId}`;
    const oidcConfig: OIDCConfig = {
      provider: SSOProvider.AZURE_AD,
      protocol: AuthProtocol.OIDC,
      enabled: true,
      clientId: this.config.clientId,
      clientSecret: this.config.clientSecret,
      callbackUrl: this.config.callbackUrl,
      discoveryUrl: `${authority}/v2.0/.well-known/openid-configuration`,
      issuer: `${authority}/v2.0`,
      jwksUri: `${authority}/discovery/v2.0/keys`,
      authorizationUrl: `${authority}/oauth2/v2.0/authorize`,
      tokenUrl: `${authority}/oauth2/v2.0/token`,
      userinfoUrl: 'https://graph.microsoft.com/v1.0/me',
      scope: this.config.scopes || AzureADProvider.DEFAULT_SCOPES,
      pkce: true,
      metadata: {
        revokeUrl: `${authority}/oauth2/v2.0/logout`,
        graphUrl: 'https://graph.microsoft.com/v1.0',
        domainHint: this.config.domainHint,
        loginHint: this.config.loginHint,
        prompt: this.config.prompt,
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
      provider: SSOProvider.AZURE_AD,
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
   * Get authorization URL with Azure-specific parameters
   */
  async getAuthorizationUrl(): Promise<{ url: string; state?: string; id?: string }> {
    if (this.protocol === AuthProtocol.OIDC || this.protocol === AuthProtocol.OAUTH2) {
      const oidcService = this.service as OIDCService;
      const { url, state } = oidcService.generateAuthorizationUrl();

      // Add Azure-specific parameters
      const urlObj = new URL(url);
      if (this.config.domainHint) {
        urlObj.searchParams.set('domain_hint', this.config.domainHint);
      }
      if (this.config.loginHint) {
        urlObj.searchParams.set('login_hint', this.config.loginHint);
      }
      if (this.config.prompt) {
        urlObj.searchParams.set('prompt', this.config.prompt);
      }

      return { url: urlObj.toString(), state };
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
    if (this.protocol === AuthProtocol.OIDC || this.protocol === AuthProtocol.OAUTH2) {
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

    // Get additional profile info from Microsoft Graph
    const userProfile = await this.getUserProfileFromGraph(tokens.accessToken);

    // Merge user data
    const user: SSOUser = {
      ...validation.user,
      ...userProfile,
    };

    return {
      user,
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
   * Get user profile from Microsoft Graph API
   */
  private async getUserProfileFromGraph(accessToken: string): Promise<Partial<SSOUser>> {
    const axios = require('axios');
    try {
      const response = await axios.get('https://graph.microsoft.com/v1.0/me', {
        headers: {
          Authorization: `Bearer ${accessToken}`,
        },
      });

      const profile = response.data;

      return {
        id: profile.id,
        email: profile.mail || profile.userPrincipalName,
        name: profile.displayName,
        firstName: profile.givenName,
        lastName: profile.surname,
        attributes: {
          jobTitle: profile.jobTitle,
          officeLocation: profile.officeLocation,
          mobilePhone: profile.mobilePhone,
          businessPhones: profile.businessPhones,
          preferredLanguage: profile.preferredLanguage,
        },
      };
    } catch (error) {
      console.error('Failed to fetch Microsoft Graph profile:', error);
      return {};
    }
  }

  /**
   * Get user's Azure AD groups
   */
  async getUserGroups(accessToken: string): Promise<string[]> {
    const axios = require('axios');
    try {
      const response = await axios.get(
        'https://graph.microsoft.com/v1.0/me/memberOf',
        {
          headers: {
            Authorization: `Bearer ${accessToken}`,
          },
        }
      );

      const groups = response.data.value || [];
      return groups.map((g: any) => g.displayName || g.id);
    } catch (error) {
      console.error('Failed to fetch Azure AD groups:', error);
      return [];
    }
  }

  /**
   * Get user profile
   */
  async getUserProfile(accessToken: string): Promise<SSOUser> {
    if (this.protocol === AuthProtocol.OIDC || this.protocol === AuthProtocol.OAUTH2) {
      const oidcService = this.service as OIDCService;
      const baseUser = await oidcService.getUserInfo(accessToken);
      const graphProfile = await this.getUserProfileFromGraph(accessToken);
      const groups = await this.getUserGroups(accessToken);

      return {
        ...baseUser,
        ...graphProfile,
        groups,
      };
    }
    throw new Error('User profile not available for SAML');
  }

  /**
   * Refresh access token
   */
  async refreshToken(refreshToken: string): Promise<{
    accessToken: string;
    refreshToken?: string;
    expiresIn: number;
  }> {
    if (this.protocol === AuthProtocol.OIDC || this.protocol === AuthProtocol.OAUTH2) {
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
  async getLogoutUrl(token: string, redirectUri?: string): Promise<string> {
    if (this.protocol === AuthProtocol.OIDC || this.protocol === AuthProtocol.OAUTH2) {
      const oidcService = this.service as OIDCService;
      return oidcService.logout(token, redirectUri);
    } else {
      const samlService = this.service as SAMLService;
      return samlService.generateLogoutRequest(token);
    }
  }

  /**
   * Validate tenant membership
   */
  validateTenantId(idToken: string): boolean {
    const jwt = require('jsonwebtoken');
    try {
      const decoded = jwt.decode(idToken) as any;
      const tokenTenantId = decoded.tid;

      // Allow 'common' tenant to accept any tenant
      if (this.config.tenantId === 'common' ||
          this.config.tenantId === 'organizations' ||
          this.config.tenantId === 'consumers') {
        return true;
      }

      return tokenTenantId === this.config.tenantId;
    } catch (error) {
      return false;
    }
  }

  /**
   * Get SAML metadata
   */
  async getServiceProviderMetadata(): Promise<string> {
    if (this.protocol === AuthProtocol.SAML) {
      const samlService = this.service as SAMLService;
      return samlService.generateServiceProviderMetadata();
    }
    throw new Error('Metadata only available for SAML');
  }
}
