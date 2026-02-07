/**
 * OpenID Connect (OIDC) Authentication Service
 * Extends OAuth2 with ID token validation and userinfo endpoint
 */

import axios from 'axios';
import * as jwt from 'jsonwebtoken';
import * as jwksClient from 'jwks-rsa';
import { OAuth2Service } from '../oauth/oauth.service';
import { OIDCConfig, SSOUser, TokenValidationResult } from '../types';

export interface OIDCDiscoveryDocument {
  issuer: string;
  authorization_endpoint: string;
  token_endpoint: string;
  userinfo_endpoint?: string;
  jwks_uri: string;
  response_types_supported: string[];
  subject_types_supported: string[];
  id_token_signing_alg_values_supported: string[];
  scopes_supported?: string[];
  token_endpoint_auth_methods_supported?: string[];
  claims_supported?: string[];
  end_session_endpoint?: string;
}

export class OIDCService extends OAuth2Service {
  private oidcConfig: OIDCConfig;
  private jwksClient: jwksClient.JwksClient;
  private discoveryDoc?: OIDCDiscoveryDocument;

  constructor(config: OIDCConfig) {
    super(config);
    this.oidcConfig = config;
    this.jwksClient = jwksClient.default({
      jwksUri: config.jwksUri,
      cache: true,
      cacheMaxAge: 3600000, // 1 hour
      rateLimit: true,
      jwksRequestsPerMinute: 10,
    });
  }

  /**
   * Discover OIDC configuration from well-known endpoint
   */
  async discover(): Promise<OIDCDiscoveryDocument> {
    if (this.discoveryDoc) {
      return this.discoveryDoc;
    }

    try {
      const response = await axios.get<OIDCDiscoveryDocument>(
        this.oidcConfig.discoveryUrl
      );
      this.discoveryDoc = response.data;
      return this.discoveryDoc;
    } catch (error) {
      throw new Error(
        `OIDC discovery failed: ${error instanceof Error ? error.message : 'Unknown error'}`
      );
    }
  }

  /**
   * Validate ID token (JWT) and extract claims
   */
  async validateIdToken(idToken: string): Promise<TokenValidationResult> {
    try {
      // Decode header to get key ID
      const decoded = jwt.decode(idToken, { complete: true });
      if (!decoded || !decoded.header.kid) {
        return {
          valid: false,
          error: 'Invalid ID token format',
        };
      }

      // Get signing key
      const key = await this.getSigningKey(decoded.header.kid);

      // Verify token
      const payload = jwt.verify(idToken, key, {
        algorithms: ['RS256', 'RS384', 'RS512'],
        issuer: this.oidcConfig.issuer,
        audience: this.oidcConfig.clientId,
      }) as jwt.JwtPayload;

      // Validate token claims
      const now = Math.floor(Date.now() / 1000);
      if (payload.exp && payload.exp < now) {
        return {
          valid: false,
          error: 'ID token has expired',
        };
      }

      if (payload.nbf && payload.nbf > now) {
        return {
          valid: false,
          error: 'ID token not yet valid',
        };
      }

      // Map claims to user
      const user = this.mapClaimsToUser(payload);

      return {
        valid: true,
        user,
        expiresAt: payload.exp ? new Date(payload.exp * 1000) : undefined,
      };
    } catch (error) {
      return {
        valid: false,
        error: error instanceof Error ? error.message : 'ID token validation failed',
      };
    }
  }

  /**
   * Fetch user information from userinfo endpoint
   */
  async getUserInfo(accessToken: string): Promise<SSOUser> {
    try {
      const userinfoUrl =
        this.oidcConfig.userinfoUrl ||
        (await this.discover()).userinfo_endpoint;

      if (!userinfoUrl) {
        throw new Error('Userinfo endpoint not available');
      }

      const response = await axios.get(userinfoUrl, {
        headers: {
          Authorization: `Bearer ${accessToken}`,
        },
      });

      return this.mapClaimsToUser(response.data);
    } catch (error) {
      throw new Error(
        `Failed to fetch user info: ${error instanceof Error ? error.message : 'Unknown error'}`
      );
    }
  }

  /**
   * Logout user from OIDC provider
   */
  async logout(idToken: string, redirectUri?: string): Promise<string> {
    const discoveryDoc = await this.discover();
    const endSessionEndpoint = discoveryDoc.end_session_endpoint;

    if (!endSessionEndpoint) {
      throw new Error('End session endpoint not available');
    }

    const params = new URLSearchParams({
      id_token_hint: idToken,
      post_logout_redirect_uri: redirectUri || this.oidcConfig.logoutUrl || this.oidcConfig.callbackUrl,
    });

    return `${endSessionEndpoint}?${params.toString()}`;
  }

  /**
   * Verify ID token at hash (for hybrid/implicit flows)
   */
  verifyAtHash(idToken: string, accessToken: string): boolean {
    try {
      const decoded = jwt.decode(idToken) as jwt.JwtPayload;
      if (!decoded.at_hash) {
        return true; // at_hash is optional
      }

      const crypto = require('crypto');
      const hash = crypto
        .createHash('sha256')
        .update(accessToken)
        .digest();
      const leftHalf = hash.slice(0, hash.length / 2);
      const atHash = leftHalf.toString('base64url');

      return atHash === decoded.at_hash;
    } catch (error) {
      return false;
    }
  }

  private async getSigningKey(kid: string): Promise<string> {
    return new Promise((resolve, reject) => {
      this.jwksClient.getSigningKey(kid, (err, key) => {
        if (err) {
          reject(err);
        } else {
          const signingKey = key?.getPublicKey();
          resolve(signingKey || '');
        }
      });
    });
  }

  private mapClaimsToUser(claims: any): SSOUser {
    return {
      id: claims.sub || claims.id || claims.email,
      email: claims.email,
      name: claims.name || `${claims.given_name || ''} ${claims.family_name || ''}`.trim(),
      firstName: claims.given_name,
      lastName: claims.family_name,
      provider: this.oidcConfig.provider,
      protocol: this.oidcConfig.protocol,
      roles: claims.roles || claims.role || [],
      groups: claims.groups || [],
      attributes: {
        preferredUsername: claims.preferred_username,
        picture: claims.picture,
        locale: claims.locale,
        zoneinfo: claims.zoneinfo,
        updatedAt: claims.updated_at,
      },
      rawProfile: claims,
    };
  }
}
