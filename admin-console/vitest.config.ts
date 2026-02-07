import { defineConfig } from 'vitest/config';
import react from '@vitejs/plugin-react';
import path from 'path';

/**
 * Vitest configuration for comprehensive integration testing
 */
export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
    },
  },
  test: {
    globals: true,
    environment: 'happy-dom',
    setupFiles: ['./tests/setup.ts'],

    // Test discovery and execution
    include: ['tests/**/*.test.ts'],
    exclude: ['node_modules', 'dist'],

    // Timeout settings (important for integration tests)
    testTimeout: 30000,
    hookTimeout: 30000,
    teardownTimeout: 10000,

    // Coverage settings
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      reportDir: './coverage',
      include: ['src/**/*.{ts,tsx}'],
      exclude: [
        'node_modules/',
        'tests/',
        'dist/',
        '**/*.d.ts',
        '**/*.config.ts',
        '**/index.ts',
      ],
      lines: 80,
      functions: 80,
      branches: 75,
      statements: 80,
      all: true,
      watermarks: {
        lines: [75, 90],
        functions: [75, 90],
        branches: [70, 85],
        statements: [75, 90],
      },
    },

    // Reporters
    reporters: ['verbose', 'html', 'json'],
    outputFile: {
      html: './test-results/report.html',
      json: './test-results/report.json',
    },

    // Concurrency settings
    threads: true,
    maxThreads: 4,
    minThreads: 1,
    isolate: true,
    isolateMemoryPerWorker: 2048,

    // Test retry and flakiness handling
    retry: 1,
    flakiTimeout: 60000,

    // Bail on first failure (optional)
    bail: 0,

    // Snapshot settings
    snapshotFormat: {
      printBasicPrototype: false,
    },

    // Globals
    globalSetup: [],
    setupFiles: ['./tests/setup.ts'],

    // Transformations
    transformMode: {
      web: [/\.[jt]sx?$/],
    },
  },
  server: {
    port: 3000,
  },
});
