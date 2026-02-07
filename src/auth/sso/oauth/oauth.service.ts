/**
 * OAuth2 Authentication Service
 * Implements OAuth 2.0 authorization code flow with PKCE support
 */

import axios from 'axios';
import * as crypto from 'crypto';
import { OAuth2Config, SSOUser, TokenValidationResult } from '../types';

export interface OAuth2Tokens {
  accessToken: string;
  refreshToken?: string;
  idToken?: string;
  expiresIn: number;
  tokenType: string;
  scope?: string;
}

export class OAuth2Service {
  private config: OAuth2Config;
  private codeVerifierMap: Map<string, string> = new Map();

  constructor(config: OAuth2Config) {
    this.config = config;
  }

  /**
   * Generate authorization URL with state and optional PKCE
   */
  generateAuthorizationUrl(redirectUri?: string): {
    url: string;
    state: string;
    codeVerifier?: string;
  } {
    const state = this.generateState();
    const params: Record<string, string> = {
      response_type: 'code',
      client_id: this.config.clientId,
      redirect_uri: redirectUri || this.config.callbackUrl,
      scope: this.config.scope.join(' '),
      state,
    };

    let codeVerifier: string | undefined;
    if (this.config.pkce) {
      codeVerifier = this.generateCodeVerifier();
      const codeChallenge = this.generateCodeChallenge(codeVerifier);
      params.code_challenge = codeChallenge;
      params.code_challenge_method = 'S256';
      this.codeVerifierMap.set(state, codeVerifier);
    }

    const url = `${this.config.authorizationUrl}?${new URLSearchParams(params).toString()}`;

    return { url, state, codeVerifier };
  }

  /**
   * Exchange authorization code for tokens
   */
  async exchangeCodeForTokens(
    code: string,
    state: string,
    redirectUri?: string
  ): Promise<OAuth2Tokens> {
    const params: Record<string, string> = {
      grant_type: 'authorization_code',
      code,
      client_id: this.config.clientId,
      redirect_uri: redirectUri || this.config.callbackUrl,
    };

    if (this.config.clientSecret) {
      params.client_secret = this.config.clientSecret;
    }

    if (this.config.pkce) {
      const codeVerifier = this.codeVerifierMap.get(state);
      if (codeVerifier) {
        params.code_verifier = codeVerifier;
        this.codeVerifierMap.delete(state);
      }
    }

    try {
      const response = await axios.post(this.config.tokenUrl, params, {
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
          Accept: 'application/json',
        },
      });

      const data = response.data;

      return {
        accessToken: data.access_token,
        refreshToken: data.refresh_token,
        idToken: data.id_token,
        expiresIn: data.expires_in,
        tokenType: data.token_type || 'Bearer',
        scope: data.scope,
      };
    } catch (error) {
      if (axios.isAxiosError(error)) {
        throw new Error(
          `Token exchange failed: ${error.response?.data?.error_description || error.message}`
        );
      }
      throw error;
    }
  }

  /**
   * Refresh access token using refresh token
   */
  async refreshAccessToken(refreshToken: string): Promise<OAuth2Tokens> {
    const params: Record<string, string> = {
      grant_type: 'refresh_token',
      refresh_token: refreshToken,
      client_id: this.config.clientId,
    };

    if (this.config.clientSecret) {
      params.client_secret = this.config.clientSecret;
    }

    try {
      const response = await axios.post(this.config.tokenUrl, params, {
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
          Accept: 'application/json',
        },
      });

      const data = response.data;

      return {
        accessToken: data.access_token,
        refreshToken: data.refresh_token || refreshToken,
        idToken: data.id_token,
        expiresIn: data.expires_in,
        tokenType: data.token_type || 'Bearer',
        scope: data.scope,
      };
    } catch (error) {
      if (axios.isAxiosError(error)) {
        throw new Error(
          `Token refresh failed: ${error.response?.data?.error_description || error.message}`
        );
      }
      throw error;
    }
  }

  /**
   * Revoke access or refresh token
   */
  async revokeToken(
    token: string,
    tokenTypeHint: 'access_token' | 'refresh_token' = 'access_token'
  ): Promise<boolean> {
    try {
      // Note: Revocation endpoint varies by provider
      const revokeUrl = this.config.metadata?.revokeUrl as string;
      if (!revokeUrl) {
        throw new Error('Revocation endpoint not configured');
      }

      await axios.post(
        revokeUrl,
        {
          token,
          token_type_hint: tokenTypeHint,
          client_id: this.config.clientId,
          client_secret: this.config.clientSecret,
        },
        {
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
        }
      );

      return true;
    } catch (error) {
      console.error('Token revocation failed:', error);
      return false;
    }
  }

  /**
   * Validate state parameter to prevent CSRF
   */
  validateState(receivedState: string, expectedState: string): boolean {
    return receivedState === expectedState;
  }

  /**
   * Validate access token (introspection)
   */
  async validateAccessToken(accessToken: string): Promise<TokenValidationResult> {
    try {
      const introspectUrl = this.config.metadata?.introspectUrl as string;
      if (!introspectUrl) {
        throw new Error('Introspection endpoint not configured');
      }

      const response = await axios.post(
        introspectUrl,
        {
          token: accessToken,
          client_id: this.config.clientId,
          client_secret: this.config.clientSecret,
        },
        {
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
        }
      );

      const data = response.data;

      if (!data.active) {
        return {
          valid: false,
          error: 'Token is not active',
        };
      }

      return {
        valid: true,
        expiresAt: new Date(data.exp * 1000),
      };
    } catch (error) {
      return {
        valid: false,
        error: error instanceof Error ? error.message : 'Token validation failed',
      };
    }
  }

  private generateState(): string {
    return crypto.randomBytes(32).toString('hex');
  }

  private generateCodeVerifier(): string {
    return crypto.randomBytes(32).toString('base64url');
  }

  private generateCodeChallenge(verifier: string): string {
    return crypto
      .createHash('sha256')
      .update(verifier)
      .digest('base64url');
  }
}
