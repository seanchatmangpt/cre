/**
 * Centralized logging system for mobile app
 */

export enum LogLevel {
  DEBUG = 'DEBUG',
  INFO = 'INFO',
  WARN = 'WARN',
  ERROR = 'ERROR',
}

interface LogEntry {
  timestamp: number;
  level: LogLevel;
  tag: string;
  message: string;
  data?: any;
  error?: string;
}

class Logger {
  private logs: LogEntry[] = [];
  private maxLogs = 1000;
  private currentLevel = LogLevel.DEBUG;
  private isDevelopment = __DEV__;

  setLevel(level: LogLevel): void {
    this.currentLevel = level;
  }

  private shouldLog(level: LogLevel): boolean {
    const levels = [LogLevel.DEBUG, LogLevel.INFO, LogLevel.WARN, LogLevel.ERROR];
    return levels.indexOf(level) >= levels.indexOf(this.currentLevel);
  }

  private addLog(entry: LogEntry): void {
    this.logs.push(entry);
    if (this.logs.length > this.maxLogs) {
      this.logs = this.logs.slice(-this.maxLogs);
    }
  }

  private formatMessage(level: LogLevel, tag: string, message: string): string {
    return `[${new Date().toISOString()}] [${level}] [${tag}] ${message}`;
  }

  debug(tag: string, message: string, data?: any): void {
    if (this.shouldLog(LogLevel.DEBUG)) {
      const entry: LogEntry = {
        timestamp: Date.now(),
        level: LogLevel.DEBUG,
        tag,
        message,
        data,
      };
      this.addLog(entry);
      if (this.isDevelopment) {
        console.log(this.formatMessage(LogLevel.DEBUG, tag, message), data || '');
      }
    }
  }

  info(tag: string, message: string, data?: any): void {
    if (this.shouldLog(LogLevel.INFO)) {
      const entry: LogEntry = {
        timestamp: Date.now(),
        level: LogLevel.INFO,
        tag,
        message,
        data,
      };
      this.addLog(entry);
      console.log(this.formatMessage(LogLevel.INFO, tag, message), data || '');
    }
  }

  warn(tag: string, message: string, data?: any): void {
    if (this.shouldLog(LogLevel.WARN)) {
      const entry: LogEntry = {
        timestamp: Date.now(),
        level: LogLevel.WARN,
        tag,
        message,
        data,
      };
      this.addLog(entry);
      console.warn(this.formatMessage(LogLevel.WARN, tag, message), data || '');
    }
  }

  error(tag: string, message: string, error?: Error | any): void {
    if (this.shouldLog(LogLevel.ERROR)) {
      const entry: LogEntry = {
        timestamp: Date.now(),
        level: LogLevel.ERROR,
        tag,
        message,
        error: error?.message || error?.toString(),
      };
      this.addLog(entry);
      console.error(this.formatMessage(LogLevel.ERROR, tag, message), error || '');
    }
  }

  getLogs(
    tag?: string,
    level?: LogLevel,
    limit: number = 100
  ): LogEntry[] {
    let filtered = [...this.logs];

    if (tag) {
      filtered = filtered.filter((log) => log.tag === tag);
    }

    if (level) {
      filtered = filtered.filter((log) => log.level === level);
    }

    return filtered.slice(-limit);
  }

  clearLogs(): void {
    this.logs = [];
  }

  exportLogs(): string {
    return JSON.stringify(this.logs, null, 2);
  }
}

export const logger = new Logger();
