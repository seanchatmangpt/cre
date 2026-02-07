/**
 * AI/ML Feature Types and Interfaces
 * Comprehensive type definitions for ML components
 */

// ============= Common Types =============

export interface MLConfig {
  modelPath?: string;
  cachePath?: string;
  maxConcurrency?: number;
  enableLogging?: boolean;
}

export interface ModelMetadata {
  name: string;
  version: string;
  format: 'onnx' | 'transformers' | 'custom';
  inputShape?: number[];
  outputShape?: number[];
  createdAt: Date;
  updatedAt: Date;
}

export interface PredictionResult<T = any> {
  prediction: T;
  confidence?: number;
  metadata?: Record<string, any>;
  latency?: number;
}

// ============= LLM Types =============

export interface LLMConfig extends MLConfig {
  model: string;
  maxTokens?: number;
  temperature?: number;
  topP?: number;
  topK?: number;
  stream?: boolean;
}

export interface LLMMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
}

export interface LLMResponse {
  text: string;
  tokens?: number;
  finishReason?: 'stop' | 'length' | 'error';
  model: string;
}

export interface LLMStreamChunk {
  token: string;
  index: number;
  done: boolean;
}

// ============= Embeddings Types =============

export interface EmbeddingConfig extends MLConfig {
  model: string;
  dimensions: number;
  normalize?: boolean;
  batchSize?: number;
}

export interface Embedding {
  vector: number[];
  text: string;
  metadata?: Record<string, any>;
  id?: string;
}

export interface SimilarityResult {
  id: string;
  score: number;
  text: string;
  metadata?: Record<string, any>;
}

// ============= Search Types =============

export interface SearchConfig {
  indexType: 'hnsw' | 'flat' | 'ivf';
  dimensions: number;
  metric: 'cosine' | 'euclidean' | 'dot';
  efConstruction?: number;
  efSearch?: number;
  M?: number;
}

export interface SearchQuery {
  vector: number[];
  topK?: number;
  threshold?: number;
  filter?: Record<string, any>;
}

export interface SearchIndex {
  add(embeddings: Embedding[]): Promise<void>;
  search(query: SearchQuery): Promise<SimilarityResult[]>;
  remove(ids: string[]): Promise<void>;
  save(path: string): Promise<void>;
  load(path: string): Promise<void>;
}

// ============= Recommendation Types =============

export interface RecommendationConfig {
  strategy: 'collaborative' | 'content-based' | 'hybrid';
  minSimilarity?: number;
  maxRecommendations?: number;
}

export interface UserProfile {
  userId: string;
  interactions: Interaction[];
  preferences?: Record<string, any>;
}

export interface Interaction {
  itemId: string;
  type: 'view' | 'like' | 'purchase' | 'rating';
  value?: number;
  timestamp: Date;
}

export interface Recommendation {
  itemId: string;
  score: number;
  reason?: string;
  confidence?: number;
}

// ============= Anomaly Detection Types =============

export interface AnomalyConfig {
  method: 'statistical' | 'isolation-forest' | 'autoencoder';
  threshold?: number;
  contamination?: number;
}

export interface AnomalyResult {
  isAnomaly: boolean;
  score: number;
  threshold: number;
  explanation?: string;
}

export interface TimeSeriesPoint {
  timestamp: Date;
  value: number;
  features?: Record<string, number>;
}

// ============= Predictive Analytics Types =============

export interface PredictiveConfig {
  type: 'time-series' | 'regression' | 'classification';
  horizon?: number;
  features?: string[];
}

export interface TimeSeriesForecast {
  timestamps: Date[];
  values: number[];
  confidence?: {
    lower: number[];
    upper: number[];
  };
  metrics?: ForecastMetrics;
}

export interface ForecastMetrics {
  mae?: number;
  rmse?: number;
  mape?: number;
  r2?: number;
}

export interface RegressionResult {
  predictions: number[];
  coefficients?: number[];
  intercept?: number;
  metrics?: RegressionMetrics;
}

export interface RegressionMetrics {
  r2: number;
  mae: number;
  rmse: number;
  mse: number;
}

// ============= Model Management Types =============

export interface ModelInfo {
  id: string;
  name: string;
  type: string;
  version: string;
  path: string;
  metadata: ModelMetadata;
  loaded: boolean;
}

export interface ModelRegistry {
  register(info: ModelInfo): Promise<void>;
  get(id: string): Promise<ModelInfo | null>;
  list(): Promise<ModelInfo[]>;
  unregister(id: string): Promise<void>;
}

// ============= Error Types =============

export class MLError extends Error {
  constructor(
    message: string,
    public code: string,
    public details?: any
  ) {
    super(message);
    this.name = 'MLError';
  }
}

export class ModelLoadError extends MLError {
  constructor(message: string, details?: any) {
    super(message, 'MODEL_LOAD_ERROR', details);
    this.name = 'ModelLoadError';
  }
}

export class InferenceError extends MLError {
  constructor(message: string, details?: any) {
    super(message, 'INFERENCE_ERROR', details);
    this.name = 'InferenceError';
  }
}
