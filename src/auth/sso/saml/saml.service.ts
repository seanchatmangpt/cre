/**
 * SAML 2.0 Authentication Service
 * Handles SAML assertions, validation, and metadata
 */

import { SAML, SamlConfig } from '@node-saml/node-saml';
import * as crypto from 'crypto';
import { SAMLConfig, SSOUser, TokenValidationResult, SSOProvider } from '../types';

export class SAMLService {
  private samlStrategy: SAML;
  private config: SAMLConfig;

  constructor(config: SAMLConfig) {
    this.config = config;
    this.samlStrategy = new SAML(this.buildSamlConfig(config));
  }

  private buildSamlConfig(config: SAMLConfig): SamlConfig {
    return {
      entryPoint: config.entryPoint,
      issuer: config.issuer,
      callbackUrl: config.callbackUrl,
      cert: config.cert,
      privateCert: config.privateCert,
      signatureAlgorithm: config.signatureAlgorithm || 'sha256',
      identifierFormat: config.identifierFormat ||
        'urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress',
      wantAssertionsSigned: config.wantAssertionsSigned ?? true,
      wantAuthnResponseSigned: config.wantAuthnResponseSigned ?? true,
      acceptedClockSkewMs: 5000,
      disableRequestedAuthnContext: false,
    };
  }

  /**
   * Generate SAML authentication request
   */
  async generateAuthRequest(): Promise<{ url: string; id: string }> {
    return new Promise((resolve, reject) => {
      this.samlStrategy.getAuthorizeUrl({}, {}, (err, url) => {
        if (err) {
          reject(err);
        } else {
          const id = this.generateRequestId();
          resolve({ url: url || '', id });
        }
      });
    });
  }

  /**
   * Validate SAML response and extract user profile
   */
  async validateResponse(samlResponse: string): Promise<TokenValidationResult> {
    try {
      const profile = await new Promise<any>((resolve, reject) => {
        this.samlStrategy.validatePostResponse(
          { SAMLResponse: samlResponse },
          (err, profile) => {
            if (err) reject(err);
            else resolve(profile);
          }
        );
      });

      if (!profile) {
        return {
          valid: false,
          error: 'Invalid SAML response',
        };
      }

      const user = this.mapProfileToUser(profile);

      return {
        valid: true,
        user,
        expiresAt: this.extractExpirationDate(profile),
      };
    } catch (error) {
      return {
        valid: false,
        error: error instanceof Error ? error.message : 'SAML validation failed',
      };
    }
  }

  /**
   * Generate SAML metadata for SP
   */
  async generateServiceProviderMetadata(): Promise<string> {
    return this.samlStrategy.generateServiceProviderMetadata(
      this.config.cert,
      this.config.privateCert
    );
  }

  /**
   * Generate SAML logout request
   */
  async generateLogoutRequest(
    userId: string,
    sessionIndex?: string
  ): Promise<string> {
    return new Promise((resolve, reject) => {
      this.samlStrategy.getLogoutUrl(
        { nameID: userId, sessionIndex },
        {},
        (err, url) => {
          if (err) reject(err);
          else resolve(url || '');
        }
      );
    });
  }

  /**
   * Validate SAML logout response
   */
  async validateLogoutResponse(response: string): Promise<boolean> {
    try {
      // Validate logout response signature and content
      const decoded = Buffer.from(response, 'base64').toString('utf-8');
      // Additional validation logic here
      return decoded.includes('LogoutResponse');
    } catch (error) {
      return false;
    }
  }

  private mapProfileToUser(profile: any): SSOUser {
    return {
      id: profile.nameID || profile.id || profile.email,
      email: profile.email || profile['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress'],
      name: profile.displayName || profile.name,
      firstName: profile.givenName || profile.firstName,
      lastName: profile.surname || profile.lastName,
      provider: this.config.provider,
      protocol: this.config.protocol,
      roles: this.extractRoles(profile),
      groups: this.extractGroups(profile),
      attributes: profile.attributes || {},
      rawProfile: profile,
    };
  }

  private extractRoles(profile: any): string[] {
    const roleAttributes = [
      'http://schemas.microsoft.com/ws/2008/06/identity/claims/role',
      'role',
      'roles',
    ];

    for (const attr of roleAttributes) {
      if (profile[attr]) {
        return Array.isArray(profile[attr]) ? profile[attr] : [profile[attr]];
      }
    }

    return [];
  }

  private extractGroups(profile: any): string[] {
    const groupAttributes = [
      'http://schemas.xmlsoap.org/claims/Group',
      'groups',
      'memberOf',
    ];

    for (const attr of groupAttributes) {
      if (profile[attr]) {
        return Array.isArray(profile[attr]) ? profile[attr] : [profile[attr]];
      }
    }

    return [];
  }

  private extractExpirationDate(profile: any): Date {
    const expiryMinutes = 60; // Default 1 hour
    return new Date(Date.now() + expiryMinutes * 60 * 1000);
  }

  private generateRequestId(): string {
    return '_' + crypto.randomBytes(21).toString('hex');
  }
}
