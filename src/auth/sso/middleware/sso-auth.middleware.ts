/**
 * SSO Authentication Middleware
 * Express middleware for protecting routes with SSO authentication
 */

import { Request, Response, NextFunction } from 'express';
import { SessionManager } from '../utils/session-manager';
import { SSOSession, SSOUser } from '../types';

// Extend Express Request type
declare global {
  namespace Express {
    interface Request {
      ssoSession?: SSOSession;
      ssoUser?: SSOUser;
    }
  }
}

export interface SSOMiddlewareOptions {
  sessionManager: SessionManager;
  sessionCookieName?: string;
  redirectUrl?: string;
  requireAuth?: boolean;
}

/**
 * Create SSO authentication middleware
 */
export function createSSOMiddleware(options: SSOMiddlewareOptions) {
  const {
    sessionManager,
    sessionCookieName = 'sso_session',
    redirectUrl = '/auth/sso/login',
    requireAuth = true,
  } = options;

  return async (req: Request, res: Response, next: NextFunction): Promise<void> => {
    try {
      // Get session ID from cookie
      const sessionId = req.cookies?.[sessionCookieName];

      if (!sessionId) {
        if (requireAuth) {
          res.status(401).json({
            error: 'Unauthorized',
            message: 'No session found',
            redirectUrl,
          });
          return;
        }
        next();
        return;
      }

      // Validate session
      const validation = await sessionManager.validateSession(sessionId);

      if (!validation.valid || !validation.session) {
        if (requireAuth) {
          res.status(401).json({
            error: 'Unauthorized',
            message: validation.error || 'Invalid session',
            redirectUrl,
          });
          return;
        }
        next();
        return;
      }

      // Attach session to request
      req.ssoSession = validation.session;

      // TODO: Load full user object from database using session.userId
      // For now, we'll create a minimal user object
      req.ssoUser = {
        id: validation.session.userId,
        email: '', // Load from database
        provider: validation.session.provider,
        protocol: validation.session.protocol,
      };

      next();
    } catch (error) {
      console.error('SSO middleware error:', error);
      res.status(500).json({
        error: 'Internal Server Error',
        message: 'Failed to validate session',
      });
    }
  };
}

/**
 * Optional authentication middleware (doesn't reject unauthenticated requests)
 */
export function optionalSSOAuth(options: Omit<SSOMiddlewareOptions, 'requireAuth'>) {
  return createSSOMiddleware({ ...options, requireAuth: false });
}

/**
 * Required authentication middleware (rejects unauthenticated requests)
 */
export function requireSSOAuth(options: Omit<SSOMiddlewareOptions, 'requireAuth'>) {
  return createSSOMiddleware({ ...options, requireAuth: true });
}

/**
 * Role-based access control middleware
 */
export function requireRole(roles: string | string[]) {
  const requiredRoles = Array.isArray(roles) ? roles : [roles];

  return (req: Request, res: Response, next: NextFunction): void => {
    const user = req.ssoUser;

    if (!user) {
      res.status(401).json({
        error: 'Unauthorized',
        message: 'Authentication required',
      });
      return;
    }

    const userRoles = user.roles || [];
    const hasRole = requiredRoles.some((role) => userRoles.includes(role));

    if (!hasRole) {
      res.status(403).json({
        error: 'Forbidden',
        message: `Required roles: ${requiredRoles.join(', ')}`,
      });
      return;
    }

    next();
  };
}

/**
 * Group membership middleware
 */
export function requireGroup(groups: string | string[]) {
  const requiredGroups = Array.isArray(groups) ? groups : [groups];

  return (req: Request, res: Response, next: NextFunction): void => {
    const user = req.ssoUser;

    if (!user) {
      res.status(401).json({
        error: 'Unauthorized',
        message: 'Authentication required',
      });
      return;
    }

    const userGroups = user.groups || [];
    const hasGroup = requiredGroups.some((group) => userGroups.includes(group));

    if (!hasGroup) {
      res.status(403).json({
        error: 'Forbidden',
        message: `Required groups: ${requiredGroups.join(', ')}`,
      });
      return;
    }

    next();
  };
}

/**
 * Provider-specific middleware
 */
export function requireProvider(providers: string | string[]) {
  const requiredProviders = Array.isArray(providers) ? providers : [providers];

  return (req: Request, res: Response, next: NextFunction): void => {
    const user = req.ssoUser;

    if (!user) {
      res.status(401).json({
        error: 'Unauthorized',
        message: 'Authentication required',
      });
      return;
    }

    if (!requiredProviders.includes(user.provider)) {
      res.status(403).json({
        error: 'Forbidden',
        message: `Required providers: ${requiredProviders.join(', ')}`,
      });
      return;
    }

    next();
  };
}
