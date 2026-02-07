/**
 * SSO Authentication Controller
 * Handles SSO login, callback, and logout endpoints
 */

import { Request, Response, Router } from 'express';
import { ProviderRegistry } from '../config/provider-registry';
import { SessionManager } from '../utils/session-manager';
import { SSOProvider, AuthProtocol, SSOCallbackData } from '../types';

export interface SSOControllerOptions {
  providerRegistry: ProviderRegistry;
  sessionManager: SessionManager;
  sessionCookieName?: string;
  sessionSecure?: boolean;
  csrfProtection?: boolean;
}

export class SSOController {
  private providerRegistry: ProviderRegistry;
  private sessionManager: SessionManager;
  private sessionCookieName: string;
  private sessionSecure: boolean;
  private csrfProtection: boolean;
  private stateStore: Map<string, { provider: SSOProvider; timestamp: number }>;

  constructor(options: SSOControllerOptions) {
    this.providerRegistry = options.providerRegistry;
    this.sessionManager = options.sessionManager;
    this.sessionCookieName = options.sessionCookieName || 'sso_session';
    this.sessionSecure = options.sessionSecure ?? true;
    this.csrfProtection = options.csrfProtection ?? true;
    this.stateStore = new Map();

    // Cleanup expired states every 10 minutes
    setInterval(() => this.cleanupStates(), 10 * 60 * 1000);
  }

  /**
   * Create Express router with SSO routes
   */
  createRouter(): Router {
    const router = Router();

    // List available providers
    router.get('/providers', this.listProviders.bind(this));

    // Initiate SSO login
    router.get('/login/:provider', this.initiateLogin.bind(this));

    // SSO callback (OAuth2/OIDC)
    router.get('/callback/:provider', this.handleCallback.bind(this));

    // SSO callback (SAML POST)
    router.post('/callback/:provider', this.handleSAMLCallback.bind(this));

    // Logout
    router.post('/logout', this.logout.bind(this));

    // Get current user session
    router.get('/session', this.getSession.bind(this));

    // Refresh token
    router.post('/refresh', this.refreshToken.bind(this));

    return router;
  }

  /**
   * List available SSO providers
   */
  private listProviders(req: Request, res: Response): void {
    const providers = this.providerRegistry.getEnabledProviders().map((reg) => ({
      provider: reg.provider,
      protocol: reg.protocol,
      loginUrl: `/auth/sso/login/${reg.provider}`,
    }));

    res.json({ providers });
  }

  /**
   * Initiate SSO login flow
   */
  private async initiateLogin(req: Request, res: Response): Promise<void> {
    try {
      const providerName = req.params.provider as SSOProvider;
      const provider = this.providerRegistry.getProvider(providerName);

      // Generate authorization URL
      const authResult = await (provider as any).getAuthorizationUrl();

      // Store state for CSRF protection
      if (authResult.state) {
        this.stateStore.set(authResult.state, {
          provider: providerName,
          timestamp: Date.now(),
        });
      }

      // Redirect to provider
      res.redirect(authResult.url);
    } catch (error) {
      console.error('SSO login error:', error);
      res.status(500).json({
        error: 'Login failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      });
    }
  }

  /**
   * Handle OAuth2/OIDC callback
   */
  private async handleCallback(req: Request, res: Response): Promise<void> {
    try {
      const providerName = req.params.provider as SSOProvider;
      const { code, state, error, error_description } = req.query as Record<string, string>;

      // Check for errors
      if (error) {
        res.status(400).json({
          error,
          message: error_description || 'Authentication failed',
        });
        return;
      }

      // Validate state (CSRF protection)
      if (this.csrfProtection && state) {
        const storedState = this.stateStore.get(state);
        if (!storedState || storedState.provider !== providerName) {
          res.status(400).json({
            error: 'Invalid state',
            message: 'CSRF token validation failed',
          });
          return;
        }
        this.stateStore.delete(state);
      }

      // Get provider instance
      const provider = this.providerRegistry.getProvider(providerName);

      // Handle callback based on provider type
      const result = await (provider as any).handleCallback({ code, state });

      // Create session
      const session = await this.sessionManager.createSession(result.user, {
        accessToken: result.accessToken,
        refreshToken: result.refreshToken,
        idToken: result.idToken,
      });

      // Set session cookie
      res.cookie(this.sessionCookieName, session.sessionId, {
        httpOnly: true,
        secure: this.sessionSecure,
        sameSite: 'lax',
        maxAge: 3600000, // 1 hour
      });

      // Respond with user data
      res.json({
        success: true,
        user: result.user,
        sessionId: session.sessionId,
      });
    } catch (error) {
      console.error('SSO callback error:', error);
      res.status(500).json({
        error: 'Authentication failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      });
    }
  }

  /**
   * Handle SAML POST callback
   */
  private async handleSAMLCallback(req: Request, res: Response): Promise<void> {
    try {
      const providerName = req.params.provider as SSOProvider;
      const { SAMLResponse } = req.body;

      if (!SAMLResponse) {
        res.status(400).json({
          error: 'Missing SAML response',
          message: 'No SAMLResponse in request body',
        });
        return;
      }

      // Get provider instance
      const provider = this.providerRegistry.getProvider(providerName, AuthProtocol.SAML);

      // Handle SAML callback
      const result = await (provider as any).handleCallback({
        samlResponse: SAMLResponse,
      });

      // Create session
      const session = await this.sessionManager.createSession(result.user, {});

      // Set session cookie
      res.cookie(this.sessionCookieName, session.sessionId, {
        httpOnly: true,
        secure: this.sessionSecure,
        sameSite: 'lax',
        maxAge: 3600000,
      });

      // Respond with user data
      res.json({
        success: true,
        user: result.user,
        sessionId: session.sessionId,
      });
    } catch (error) {
      console.error('SAML callback error:', error);
      res.status(500).json({
        error: 'Authentication failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      });
    }
  }

  /**
   * Logout user
   */
  private async logout(req: Request, res: Response): Promise<void> {
    try {
      const sessionId = req.cookies?.[this.sessionCookieName];

      if (sessionId) {
        await this.sessionManager.destroySession(sessionId);
      }

      // Clear session cookie
      res.clearCookie(this.sessionCookieName);

      res.json({ success: true, message: 'Logged out successfully' });
    } catch (error) {
      console.error('Logout error:', error);
      res.status(500).json({
        error: 'Logout failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      });
    }
  }

  /**
   * Get current session
   */
  private async getSession(req: Request, res: Response): Promise<void> {
    try {
      const sessionId = req.cookies?.[this.sessionCookieName];

      if (!sessionId) {
        res.status(401).json({
          error: 'No session',
          message: 'Not authenticated',
        });
        return;
      }

      const validation = await this.sessionManager.validateSession(sessionId);

      if (!validation.valid || !validation.session) {
        res.status(401).json({
          error: 'Invalid session',
          message: validation.error || 'Session expired',
        });
        return;
      }

      res.json({
        session: {
          sessionId: validation.session.sessionId,
          userId: validation.session.userId,
          provider: validation.session.provider,
          protocol: validation.session.protocol,
          expiresAt: validation.session.expiresAt,
        },
      });
    } catch (error) {
      console.error('Get session error:', error);
      res.status(500).json({
        error: 'Failed to get session',
        message: error instanceof Error ? error.message : 'Unknown error',
      });
    }
  }

  /**
   * Refresh access token
   */
  private async refreshToken(req: Request, res: Response): Promise<void> {
    try {
      const sessionId = req.cookies?.[this.sessionCookieName];

      if (!sessionId) {
        res.status(401).json({
          error: 'No session',
          message: 'Not authenticated',
        });
        return;
      }

      const session = await this.sessionManager.getSession(sessionId);

      if (!session || !session.refreshToken) {
        res.status(401).json({
          error: 'Cannot refresh',
          message: 'No refresh token available',
        });
        return;
      }

      // Get provider instance
      const provider = this.providerRegistry.getProvider(session.provider);

      // Refresh token
      const tokens = await (provider as any).refreshToken(session.refreshToken);

      // Update session with new tokens
      await this.sessionManager.updateTokens(sessionId, {
        accessToken: tokens.accessToken,
        refreshToken: tokens.refreshToken,
      });

      res.json({
        success: true,
        accessToken: tokens.accessToken,
        expiresIn: tokens.expiresIn,
      });
    } catch (error) {
      console.error('Token refresh error:', error);
      res.status(500).json({
        error: 'Refresh failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      });
    }
  }

  /**
   * Cleanup expired states
   */
  private cleanupStates(): void {
    const now = Date.now();
    const maxAge = 10 * 60 * 1000; // 10 minutes

    for (const [state, data] of this.stateStore.entries()) {
      if (now - data.timestamp > maxAge) {
        this.stateStore.delete(state);
      }
    }
  }
}
