/**
 * SSO Session Manager
 * Handles session creation, validation, and storage
 */

import * as crypto from 'crypto';
import { SSOSession, SSOUser, SSOProvider, AuthProtocol } from '../types';

export interface SessionStore {
  get(sessionId: string): Promise<SSOSession | null>;
  set(sessionId: string, session: SSOSession): Promise<void>;
  delete(sessionId: string): Promise<void>;
  cleanup(): Promise<void>;
}

/**
 * In-memory session store (for development)
 * Replace with Redis, Database, or other persistent store in production
 */
export class InMemorySessionStore implements SessionStore {
  private sessions: Map<string, SSOSession> = new Map();

  async get(sessionId: string): Promise<SSOSession | null> {
    const session = this.sessions.get(sessionId);
    if (!session) return null;

    // Check if session is expired
    if (new Date() > session.expiresAt) {
      await this.delete(sessionId);
      return null;
    }

    return session;
  }

  async set(sessionId: string, session: SSOSession): Promise<void> {
    this.sessions.set(sessionId, session);
  }

  async delete(sessionId: string): Promise<void> {
    this.sessions.delete(sessionId);
  }

  async cleanup(): Promise<void> {
    const now = new Date();
    for (const [sessionId, session] of this.sessions.entries()) {
      if (now > session.expiresAt) {
        this.sessions.delete(sessionId);
      }
    }
  }
}

export class SessionManager {
  private store: SessionStore;
  private sessionMaxAge: number;

  constructor(store: SessionStore, sessionMaxAge: number = 3600000) {
    this.store = store;
    this.sessionMaxAge = sessionMaxAge;

    // Run cleanup every 15 minutes
    setInterval(() => this.store.cleanup(), 15 * 60 * 1000);
  }

  /**
   * Create new session
   */
  async createSession(
    user: SSOUser,
    tokens: {
      accessToken?: string;
      refreshToken?: string;
      idToken?: string;
    }
  ): Promise<SSOSession> {
    const sessionId = this.generateSessionId();
    const now = new Date();
    const expiresAt = new Date(now.getTime() + this.sessionMaxAge);

    const session: SSOSession = {
      sessionId,
      userId: user.id,
      provider: user.provider,
      protocol: user.protocol,
      accessToken: tokens.accessToken,
      refreshToken: tokens.refreshToken,
      idToken: tokens.idToken,
      expiresAt,
      createdAt: now,
      lastAccessedAt: now,
    };

    await this.store.set(sessionId, session);

    return session;
  }

  /**
   * Get session by ID
   */
  async getSession(sessionId: string): Promise<SSOSession | null> {
    const session = await this.store.get(sessionId);
    if (!session) return null;

    // Update last accessed time
    session.lastAccessedAt = new Date();
    await this.store.set(sessionId, session);

    return session;
  }

  /**
   * Validate session
   */
  async validateSession(sessionId: string): Promise<{
    valid: boolean;
    session?: SSOSession;
    error?: string;
  }> {
    const session = await this.getSession(sessionId);

    if (!session) {
      return {
        valid: false,
        error: 'Session not found',
      };
    }

    const now = new Date();
    if (now > session.expiresAt) {
      await this.destroySession(sessionId);
      return {
        valid: false,
        error: 'Session expired',
      };
    }

    return {
      valid: true,
      session,
    };
  }

  /**
   * Update session tokens
   */
  async updateTokens(
    sessionId: string,
    tokens: {
      accessToken?: string;
      refreshToken?: string;
      idToken?: string;
    }
  ): Promise<void> {
    const session = await this.store.get(sessionId);
    if (!session) {
      throw new Error('Session not found');
    }

    if (tokens.accessToken) session.accessToken = tokens.accessToken;
    if (tokens.refreshToken) session.refreshToken = tokens.refreshToken;
    if (tokens.idToken) session.idToken = tokens.idToken;

    session.lastAccessedAt = new Date();

    await this.store.set(sessionId, session);
  }

  /**
   * Extend session expiration
   */
  async extendSession(sessionId: string): Promise<void> {
    const session = await this.store.get(sessionId);
    if (!session) {
      throw new Error('Session not found');
    }

    const now = new Date();
    session.expiresAt = new Date(now.getTime() + this.sessionMaxAge);
    session.lastAccessedAt = now;

    await this.store.set(sessionId, session);
  }

  /**
   * Destroy session
   */
  async destroySession(sessionId: string): Promise<void> {
    await this.store.delete(sessionId);
  }

  /**
   * Get all sessions for a user (for force logout)
   */
  async getUserSessions(userId: string): Promise<SSOSession[]> {
    // This is inefficient for large stores - implement proper indexing in production
    const sessions: SSOSession[] = [];

    // Note: This implementation is for InMemorySessionStore
    // For production, implement proper user-to-session indexing
    if (this.store instanceof InMemorySessionStore) {
      const allSessions = Array.from((this.store as any).sessions.values());
      for (const session of allSessions) {
        if (session.userId === userId) {
          sessions.push(session);
        }
      }
    }

    return sessions;
  }

  /**
   * Destroy all sessions for a user
   */
  async destroyAllUserSessions(userId: string): Promise<void> {
    const sessions = await this.getUserSessions(userId);
    await Promise.all(
      sessions.map((session) => this.destroySession(session.sessionId))
    );
  }

  private generateSessionId(): string {
    return crypto.randomBytes(32).toString('hex');
  }
}
