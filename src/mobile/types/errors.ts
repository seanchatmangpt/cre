/**
 * Custom error classes for mobile app
 */

export class AppError extends Error {
  constructor(
    public code: string,
    message: string,
    public statusCode: number = 500,
    public details?: Record<string, any>
  ) {
    super(message);
    this.name = 'AppError';
    Object.setPrototypeOf(this, AppError.prototype);
  }
}

export class NetworkError extends AppError {
  constructor(message: string = 'Network request failed') {
    super('NETWORK_ERROR', message, 0);
    this.name = 'NetworkError';
    Object.setPrototypeOf(this, NetworkError.prototype);
  }
}

export class AuthenticationError extends AppError {
  constructor(message: string = 'Authentication failed') {
    super('AUTH_ERROR', message, 401);
    this.name = 'AuthenticationError';
    Object.setPrototypeOf(this, AuthenticationError.prototype);
  }
}

export class BiometricError extends AppError {
  constructor(message: string = 'Biometric authentication failed') {
    super('BIOMETRIC_ERROR', message, 403);
    this.name = 'BiometricError';
    Object.setPrototypeOf(this, BiometricError.prototype);
  }
}

export class OfflineSyncError extends AppError {
  constructor(message: string = 'Offline sync failed') {
    super('SYNC_ERROR', message, 409);
    this.name = 'OfflineSyncError';
    Object.setPrototypeOf(this, OfflineSyncError.prototype);
  }
}

export class ConflictError extends AppError {
  constructor(
    message: string = 'Data conflict detected',
    public conflictingData?: any
  ) {
    super('CONFLICT_ERROR', message, 409);
    this.name = 'ConflictError';
    Object.setPrototypeOf(this, ConflictError.prototype);
  }
}

export class ValidationError extends AppError {
  constructor(message: string = 'Validation failed', details?: Record<string, any>) {
    super('VALIDATION_ERROR', message, 422, details);
    this.name = 'ValidationError';
    Object.setPrototypeOf(this, ValidationError.prototype);
  }
}

export class NotFoundError extends AppError {
  constructor(message: string = 'Resource not found') {
    super('NOT_FOUND', message, 404);
    this.name = 'NotFoundError';
    Object.setPrototypeOf(this, NotFoundError.prototype);
  }
}

export class TimeoutError extends AppError {
  constructor(message: string = 'Request timeout') {
    super('TIMEOUT', message, 408);
    this.name = 'TimeoutError';
    Object.setPrototypeOf(this, TimeoutError.prototype);
  }
}

export class StorageError extends AppError {
  constructor(message: string = 'Storage operation failed') {
    super('STORAGE_ERROR', message, 500);
    this.name = 'StorageError';
    Object.setPrototypeOf(this, StorageError.prototype);
  }
}

export class ARError extends AppError {
  constructor(message: string = 'AR operation failed') {
    super('AR_ERROR', message, 500);
    this.name = 'ARError';
    Object.setPrototypeOf(this, ARError.prototype);
  }
}
