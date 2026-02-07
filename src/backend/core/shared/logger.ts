/**
 * Centralized logging service with Winston and Pino
 */

import pino from 'pino';
import pinoHttp from 'pino-http';
import { ServiceConfig } from '../types';

export class Logger {
  private logger: pino.Logger;
  private config: ServiceConfig;

  constructor(config: ServiceConfig) {
    this.config = config;
    this.logger = pino({
      level: config.logLevel || 'info',
      enabled: !config.debug,
      formatters: {
        level: (label) => {
          return { level: label.toUpperCase() };
        },
      },
      transport:
        config.environment !== 'production'
          ? {
              target: 'pino-pretty',
              options: {
                colorize: true,
                translateTime: 'SYS:standard',
                ignore: 'pid,hostname',
              },
            }
          : undefined,
    });
  }

  getHttpMiddleware() {
    return pinoHttp({
      logger: this.logger,
      useLevel: this.config.logLevel || 'info',
    });
  }

  error(message: string, error?: Error, meta?: Record<string, unknown>) {
    this.logger.error({ err: error, ...meta }, message);
  }

  warn(message: string, meta?: Record<string, unknown>) {
    this.logger.warn(meta, message);
  }

  info(message: string, meta?: Record<string, unknown>) {
    this.logger.info(meta, message);
  }

  debug(message: string, meta?: Record<string, unknown>) {
    this.logger.debug(meta, message);
  }

  trace(message: string, meta?: Record<string, unknown>) {
    this.logger.trace(meta, message);
  }

  getLogger() {
    return this.logger;
  }
}

let loggerInstance: Logger;

export function initializeLogger(config: ServiceConfig): Logger {
  loggerInstance = new Logger(config);
  return loggerInstance;
}

export function getLogger(): Logger {
  if (!loggerInstance) {
    throw new Error('Logger not initialized. Call initializeLogger first.');
  }
  return loggerInstance;
}
