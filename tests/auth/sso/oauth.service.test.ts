/**
 * OAuth2 Service Tests
 */

import { OAuth2Service } from '../../../src/auth/sso/oauth/oauth.service';
import { OAuth2Config, SSOProvider, AuthProtocol } from '../../../src/auth/sso/types';
import axios from 'axios';

jest.mock('axios');
const mockedAxios = axios as jest.Mocked<typeof axios>;

describe('OAuth2Service', () => {
  let oauth2Service: OAuth2Service;
  let mockConfig: OAuth2Config;

  beforeEach(() => {
    mockConfig = {
      provider: SSOProvider.GOOGLE_WORKSPACE,
      protocol: AuthProtocol.OAUTH2,
      enabled: true,
      clientId: 'test-client-id',
      clientSecret: 'test-client-secret',
      callbackUrl: 'https://app.example.com/auth/callback',
      authorizationUrl: 'https://accounts.google.com/o/oauth2/v2/auth',
      tokenUrl: 'https://oauth2.googleapis.com/token',
      scope: ['openid', 'email', 'profile'],
      pkce: true,
    };

    oauth2Service = new OAuth2Service(mockConfig);
  });

  describe('generateAuthorizationUrl', () => {
    it('should generate authorization URL with state', () => {
      const result = oauth2Service.generateAuthorizationUrl();

      expect(result.url).toContain(mockConfig.authorizationUrl);
      expect(result.url).toContain(`client_id=${mockConfig.clientId}`);
      expect(result.url).toContain('response_type=code');
      expect(result.url).toContain('scope=openid%20email%20profile');
      expect(result.state).toMatch(/^[a-f0-9]{64}$/);
    });

    it('should include PKCE parameters when enabled', () => {
      const result = oauth2Service.generateAuthorizationUrl();

      expect(result.url).toContain('code_challenge');
      expect(result.url).toContain('code_challenge_method=S256');
      expect(result.codeVerifier).toBeDefined();
    });

    it('should not include PKCE when disabled', () => {
      mockConfig.pkce = false;
      const service = new OAuth2Service(mockConfig);
      const result = service.generateAuthorizationUrl();

      expect(result.url).not.toContain('code_challenge');
      expect(result.codeVerifier).toBeUndefined();
    });
  });

  describe('exchangeCodeForTokens', () => {
    it('should exchange authorization code for tokens', async () => {
      const mockTokenResponse = {
        data: {
          access_token: 'access-token-123',
          refresh_token: 'refresh-token-456',
          id_token: 'id-token-789',
          expires_in: 3600,
          token_type: 'Bearer',
        },
      };

      mockedAxios.post.mockResolvedValue(mockTokenResponse);

      const tokens = await oauth2Service.exchangeCodeForTokens(
        'auth-code-123',
        'state-abc'
      );

      expect(tokens.accessToken).toBe('access-token-123');
      expect(tokens.refreshToken).toBe('refresh-token-456');
      expect(tokens.idToken).toBe('id-token-789');
      expect(tokens.expiresIn).toBe(3600);
      expect(tokens.tokenType).toBe('Bearer');

      expect(mockedAxios.post).toHaveBeenCalledWith(
        mockConfig.tokenUrl,
        expect.objectContaining({
          grant_type: 'authorization_code',
          code: 'auth-code-123',
          client_id: mockConfig.clientId,
          client_secret: mockConfig.clientSecret,
        }),
        expect.any(Object)
      );
    });

    it('should handle token exchange errors', async () => {
      mockedAxios.post.mockRejectedValue({
        response: {
          data: {
            error: 'invalid_grant',
            error_description: 'Invalid authorization code',
          },
        },
        isAxiosError: true,
      });

      mockedAxios.isAxiosError = jest.fn().mockReturnValue(true);

      await expect(
        oauth2Service.exchangeCodeForTokens('invalid-code', 'state')
      ).rejects.toThrow('Token exchange failed: Invalid authorization code');
    });
  });

  describe('refreshAccessToken', () => {
    it('should refresh access token successfully', async () => {
      const mockRefreshResponse = {
        data: {
          access_token: 'new-access-token',
          expires_in: 3600,
          token_type: 'Bearer',
        },
      };

      mockedAxios.post.mockResolvedValue(mockRefreshResponse);

      const tokens = await oauth2Service.refreshAccessToken('refresh-token-123');

      expect(tokens.accessToken).toBe('new-access-token');
      expect(tokens.expiresIn).toBe(3600);

      expect(mockedAxios.post).toHaveBeenCalledWith(
        mockConfig.tokenUrl,
        expect.objectContaining({
          grant_type: 'refresh_token',
          refresh_token: 'refresh-token-123',
        }),
        expect.any(Object)
      );
    });
  });

  describe('validateState', () => {
    it('should validate matching state', () => {
      const state = 'test-state-123';
      expect(oauth2Service.validateState(state, state)).toBe(true);
    });

    it('should reject non-matching state', () => {
      expect(oauth2Service.validateState('state-1', 'state-2')).toBe(false);
    });
  });

  describe('revokeToken', () => {
    it('should revoke token successfully', async () => {
      mockConfig.metadata = {
        revokeUrl: 'https://oauth2.googleapis.com/revoke',
      };
      const service = new OAuth2Service(mockConfig);

      mockedAxios.post.mockResolvedValue({ data: {} });

      const result = await service.revokeToken('token-123');

      expect(result).toBe(true);
      expect(mockedAxios.post).toHaveBeenCalledWith(
        'https://oauth2.googleapis.com/revoke',
        expect.objectContaining({
          token: 'token-123',
        }),
        expect.any(Object)
      );
    });

    it('should handle revocation errors gracefully', async () => {
      mockConfig.metadata = {
        revokeUrl: 'https://oauth2.googleapis.com/revoke',
      };
      const service = new OAuth2Service(mockConfig);

      mockedAxios.post.mockRejectedValue(new Error('Network error'));

      const result = await service.revokeToken('token-123');

      expect(result).toBe(false);
    });
  });
});
