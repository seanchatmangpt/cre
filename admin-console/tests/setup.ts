import { expect, afterEach, vi } from 'vitest';
import '@testing-library/jest-dom';

/**
 * Global test setup for integration tests
 * Configures mocking, timers, and cleanup
 */

// Mock environment variables
process.env.VITE_API_BASE_URL = 'http://localhost:8080';
process.env.VITE_WS_URL = 'ws://localhost:8081';

// Setup global mocks
global.fetch = vi.fn();
global.WebSocket = vi.fn() as any;

// Mock localStorage
const localStorageMock = {
  getItem: vi.fn(),
  setItem: vi.fn(),
  removeItem: vi.fn(),
  clear: vi.fn(),
};
Object.defineProperty(window, 'localStorage', { value: localStorageMock });

// Mock sessionStorage
const sessionStorageMock = {
  getItem: vi.fn(),
  setItem: vi.fn(),
  removeItem: vi.fn(),
  clear: vi.fn(),
};
Object.defineProperty(window, 'sessionStorage', { value: sessionStorageMock });

// Cleanup after each test
afterEach(() => {
  vi.clearAllMocks();
  localStorageMock.clear();
  sessionStorageMock.clear();
});

// Suppress console errors during tests (optional)
const originalError = console.error;
beforeAll(() => {
  console.error = vi.fn((...args) => {
    if (
      typeof args[0] === 'string' &&
      args[0].includes('Not implemented: HTMLFormElement.prototype.submit')
    ) {
      return;
    }
    originalError.call(console, ...args);
  });
});

afterAll(() => {
  console.error = originalError;
});
