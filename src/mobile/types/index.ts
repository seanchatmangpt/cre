/**
 * Core type definitions for React Native 2030 mobile app
 * Supports offline-first architecture, biometrics, AR/VR, and real-time sync
 */

export enum SyncStatus {
  SYNCED = 'synced',
  SYNCING = 'syncing',
  PENDING = 'pending',
  ERROR = 'error',
  OFFLINE = 'offline',
}

export enum ConflictResolutionStrategy {
  LAST_WRITE_WINS = 'last_write_wins',
  FIRST_WRITE_WINS = 'first_write_wins',
  SERVER_WINS = 'server_wins',
  CLIENT_WINS = 'client_wins',
  MERGE = 'merge',
}

export interface SyncItem<T = any> {
  id: string;
  entityId: string;
  entityType: string;
  operation: 'create' | 'update' | 'delete';
  data: T;
  timestamp: number;
  status: SyncStatus;
  retries: number;
  lastError?: string;
  version: number;
  hash: string;
}

export interface User {
  id: string;
  email: string;
  displayName: string;
  avatar?: string;
  biometricEnabled: boolean;
  lastLogin: number;
  timezone: string;
  preferences: UserPreferences;
}

export interface UserPreferences {
  theme: 'light' | 'dark' | 'auto';
  language: string;
  notifications: boolean;
  deepLinks: boolean;
  offlineMode: boolean;
  arEnabled: boolean;
  deviceSize: 'phone' | 'tablet' | 'foldable';
}

export interface AuthState {
  user: User | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  error: string | null;
  token: string | null;
  refreshToken: string | null;
  sessionId: string | null;
}

export interface OfflineState {
  isOnline: boolean;
  syncQueue: SyncItem[];
  lastSync: number;
  pendingChanges: number;
  conflictingItems: SyncItem[];
}

export interface NotificationPayload {
  id: string;
  title: string;
  body: string;
  data?: Record<string, any>;
  deepLink?: string;
  image?: string;
  badge: number;
  timestamp: number;
  type: 'alert' | 'message' | 'update' | 'social';
}

export interface DeepLinkRoute {
  name: string;
  path: string;
  params?: Record<string, any>;
}

export interface ARObject {
  id: string;
  modelUri: string;
  modelType: '3d' | 'image' | 'glb' | 'usdz';
  position: ARVector3;
  rotation: ARVector3;
  scale: ARVector3;
  metadata?: Record<string, any>;
  timestamp: number;
}

export interface ARVector3 {
  x: number;
  y: number;
  z: number;
}

export interface VRScene {
  id: string;
  name: string;
  description: string;
  objects: ARObject[];
  lighting: VRLighting;
  camera: VRCamera;
}

export interface VRLighting {
  ambient: string;
  intensity: number;
  color: string;
}

export interface VRCamera {
  position: ARVector3;
  target: ARVector3;
  fov: number;
}

export interface ResponsiveSize {
  width: number;
  height: number;
  scale: number;
  isTablet: boolean;
  isFoldable: boolean;
  orientation: 'portrait' | 'landscape';
  safeAreaInsets: {
    top: number;
    bottom: number;
    left: number;
    right: number;
  };
}

export interface AppTheme {
  colors: {
    primary: string;
    secondary: string;
    background: string;
    surface: string;
    text: string;
    textSecondary: string;
    border: string;
    error: string;
    success: string;
    warning: string;
    info: string;
  };
  spacing: {
    xs: number;
    sm: number;
    md: number;
    lg: number;
    xl: number;
  };
  typography: {
    h1: { fontSize: number; fontWeight: 'bold' };
    h2: { fontSize: number; fontWeight: '600' };
    body: { fontSize: number; fontWeight: '400' };
    small: { fontSize: number; fontWeight: '400' };
  };
}

export interface AppState {
  auth: AuthState;
  offline: OfflineState;
  notifications: NotificationPayload[];
  preferences: UserPreferences;
  theme: AppTheme;
}

export interface SyncConflict {
  item: SyncItem;
  serverVersion: SyncItem;
  resolution: ConflictResolutionStrategy;
  resolvedAt: number;
}

export interface BiometricConfig {
  enabled: boolean;
  type: 'fingerprint' | 'face' | 'iris';
  fallbackPin: string | null;
  timeout: number;
}

export interface APIError {
  code: string;
  message: string;
  statusCode: number;
  details?: Record<string, any>;
  timestamp: number;
}

export interface PaginatedResponse<T> {
  data: T[];
  page: number;
  pageSize: number;
  total: number;
  hasMore: boolean;
}
