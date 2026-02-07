/**
 * ML Logger Utility
 * Structured logging for ML operations
 */

export enum LogLevel {
  DEBUG = 0,
  INFO = 1,
  WARN = 2,
  ERROR = 3,
}

export interface LogEntry {
  timestamp: Date;
  level: LogLevel;
  component: string;
  message: string;
  data?: any;
}

export class MLLogger {
  private level: LogLevel;
  private component: string;

  constructor(component: string, level: LogLevel = LogLevel.INFO) {
    this.component = component;
    this.level = level;
  }

  private log(level: LogLevel, message: string, data?: any): void {
    if (level < this.level) return;

    const entry: LogEntry = {
      timestamp: new Date(),
      level,
      component: this.component,
      message,
      data,
    };

    const levelName = LogLevel[level];
    const formatted = `[${entry.timestamp.toISOString()}] [${levelName}] [${this.component}] ${message}`;

    switch (level) {
      case LogLevel.DEBUG:
      case LogLevel.INFO:
        console.log(formatted, data || '');
        break;
      case LogLevel.WARN:
        console.warn(formatted, data || '');
        break;
      case LogLevel.ERROR:
        console.error(formatted, data || '');
        break;
    }
  }

  debug(message: string, data?: any): void {
    this.log(LogLevel.DEBUG, message, data);
  }

  info(message: string, data?: any): void {
    this.log(LogLevel.INFO, message, data);
  }

  warn(message: string, data?: any): void {
    this.log(LogLevel.WARN, message, data);
  }

  error(message: string, error?: Error | any): void {
    this.log(LogLevel.ERROR, message, error);
  }

  setLevel(level: LogLevel): void {
    this.level = level;
  }
}

export const createLogger = (component: string, level?: LogLevel): MLLogger => {
  return new MLLogger(component, level);
};
