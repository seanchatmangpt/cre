/**
 * Session Manager Tests
 */

import {
  SessionManager,
  InMemorySessionStore,
} from '../../../src/auth/sso/utils/session-manager';
import { SSOUser, SSOProvider, AuthProtocol } from '../../../src/auth/sso/types';

describe('SessionManager', () => {
  let sessionManager: SessionManager;
  let sessionStore: InMemorySessionStore;
  let mockUser: SSOUser;

  beforeEach(() => {
    sessionStore = new InMemorySessionStore();
    sessionManager = new SessionManager(sessionStore, 3600000);

    mockUser = {
      id: 'user-123',
      email: 'user@example.com',
      name: 'Test User',
      provider: SSOProvider.GOOGLE_WORKSPACE,
      protocol: AuthProtocol.OIDC,
    };
  });

  describe('createSession', () => {
    it('should create a new session', async () => {
      const tokens = {
        accessToken: 'access-token',
        refreshToken: 'refresh-token',
        idToken: 'id-token',
      };

      const session = await sessionManager.createSession(mockUser, tokens);

      expect(session.sessionId).toMatch(/^[a-f0-9]{64}$/);
      expect(session.userId).toBe(mockUser.id);
      expect(session.provider).toBe(mockUser.provider);
      expect(session.protocol).toBe(mockUser.protocol);
      expect(session.accessToken).toBe(tokens.accessToken);
      expect(session.refreshToken).toBe(tokens.refreshToken);
      expect(session.idToken).toBe(tokens.idToken);
      expect(session.expiresAt).toBeInstanceOf(Date);
      expect(session.createdAt).toBeInstanceOf(Date);
      expect(session.lastAccessedAt).toBeInstanceOf(Date);
    });
  });

  describe('getSession', () => {
    it('should retrieve existing session', async () => {
      const createdSession = await sessionManager.createSession(mockUser, {});

      const retrievedSession = await sessionManager.getSession(
        createdSession.sessionId
      );

      expect(retrievedSession).toBeDefined();
      expect(retrievedSession?.sessionId).toBe(createdSession.sessionId);
      expect(retrievedSession?.userId).toBe(mockUser.id);
    });

    it('should return null for non-existent session', async () => {
      const session = await sessionManager.getSession('non-existent-id');

      expect(session).toBeNull();
    });

    it('should update last accessed time', async () => {
      const createdSession = await sessionManager.createSession(mockUser, {});
      const originalLastAccessed = createdSession.lastAccessedAt;

      // Wait a bit
      await new Promise((resolve) => setTimeout(resolve, 10));

      const retrievedSession = await sessionManager.getSession(
        createdSession.sessionId
      );

      expect(retrievedSession?.lastAccessedAt.getTime()).toBeGreaterThan(
        originalLastAccessed.getTime()
      );
    });
  });

  describe('validateSession', () => {
    it('should validate valid session', async () => {
      const createdSession = await sessionManager.createSession(mockUser, {});

      const validation = await sessionManager.validateSession(
        createdSession.sessionId
      );

      expect(validation.valid).toBe(true);
      expect(validation.session).toBeDefined();
      expect(validation.error).toBeUndefined();
    });

    it('should reject non-existent session', async () => {
      const validation = await sessionManager.validateSession('non-existent');

      expect(validation.valid).toBe(false);
      expect(validation.error).toBe('Session not found');
    });

    it('should reject expired session', async () => {
      // Create session with very short expiry
      const shortLivedManager = new SessionManager(sessionStore, 10);
      const session = await shortLivedManager.createSession(mockUser, {});

      // Wait for expiry
      await new Promise((resolve) => setTimeout(resolve, 20));

      const validation = await shortLivedManager.validateSession(session.sessionId);

      expect(validation.valid).toBe(false);
      expect(validation.error).toBe('Session expired');
    });
  });

  describe('updateTokens', () => {
    it('should update session tokens', async () => {
      const session = await sessionManager.createSession(mockUser, {
        accessToken: 'old-token',
      });

      await sessionManager.updateTokens(session.sessionId, {
        accessToken: 'new-token',
        refreshToken: 'new-refresh-token',
      });

      const updatedSession = await sessionManager.getSession(session.sessionId);

      expect(updatedSession?.accessToken).toBe('new-token');
      expect(updatedSession?.refreshToken).toBe('new-refresh-token');
    });

    it('should throw error for non-existent session', async () => {
      await expect(
        sessionManager.updateTokens('non-existent', { accessToken: 'token' })
      ).rejects.toThrow('Session not found');
    });
  });

  describe('extendSession', () => {
    it('should extend session expiration', async () => {
      const session = await sessionManager.createSession(mockUser, {});
      const originalExpiry = session.expiresAt;

      // Wait a bit
      await new Promise((resolve) => setTimeout(resolve, 10));

      await sessionManager.extendSession(session.sessionId);

      const extendedSession = await sessionManager.getSession(session.sessionId);

      expect(extendedSession?.expiresAt.getTime()).toBeGreaterThan(
        originalExpiry.getTime()
      );
    });
  });

  describe('destroySession', () => {
    it('should destroy session', async () => {
      const session = await sessionManager.createSession(mockUser, {});

      await sessionManager.destroySession(session.sessionId);

      const retrievedSession = await sessionManager.getSession(session.sessionId);

      expect(retrievedSession).toBeNull();
    });
  });

  describe('getUserSessions', () => {
    it('should get all sessions for a user', async () => {
      await sessionManager.createSession(mockUser, {});
      await sessionManager.createSession(mockUser, {});

      const sessions = await sessionManager.getUserSessions(mockUser.id);

      expect(sessions).toHaveLength(2);
      expect(sessions.every((s) => s.userId === mockUser.id)).toBe(true);
    });
  });

  describe('destroyAllUserSessions', () => {
    it('should destroy all sessions for a user', async () => {
      await sessionManager.createSession(mockUser, {});
      await sessionManager.createSession(mockUser, {});

      await sessionManager.destroyAllUserSessions(mockUser.id);

      const sessions = await sessionManager.getUserSessions(mockUser.id);

      expect(sessions).toHaveLength(0);
    });
  });
});

describe('InMemorySessionStore', () => {
  let store: InMemorySessionStore;

  beforeEach(() => {
    store = new InMemorySessionStore();
  });

  describe('cleanup', () => {
    it('should remove expired sessions', async () => {
      const expiredSession = {
        sessionId: 'session-1',
        userId: 'user-1',
        provider: SSOProvider.GOOGLE_WORKSPACE,
        protocol: AuthProtocol.OIDC,
        expiresAt: new Date(Date.now() - 1000),
        createdAt: new Date(),
        lastAccessedAt: new Date(),
      };

      const validSession = {
        ...expiredSession,
        sessionId: 'session-2',
        expiresAt: new Date(Date.now() + 10000),
      };

      await store.set('session-1', expiredSession);
      await store.set('session-2', validSession);

      await store.cleanup();

      const expired = await store.get('session-1');
      const valid = await store.get('session-2');

      expect(expired).toBeNull();
      expect(valid).toBeDefined();
    });
  });
});
