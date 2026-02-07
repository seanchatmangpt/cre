/**
 * Model Cache Utility
 * LRU cache for model results and embeddings
 */

interface CacheEntry<T> {
  value: T;
  timestamp: number;
  hits: number;
}

export class LRUCache<T> {
  private cache: Map<string, CacheEntry<T>>;
  private maxSize: number;
  private ttl: number;

  constructor(maxSize: number = 1000, ttl: number = 3600000) {
    this.cache = new Map();
    this.maxSize = maxSize;
    this.ttl = ttl;
  }

  get(key: string): T | null {
    const entry = this.cache.get(key);

    if (!entry) return null;

    // Check TTL
    if (Date.now() - entry.timestamp > this.ttl) {
      this.cache.delete(key);
      return null;
    }

    // Update hits
    entry.hits++;
    return entry.value;
  }

  set(key: string, value: T): void {
    // Evict if at capacity
    if (this.cache.size >= this.maxSize && !this.cache.has(key)) {
      this.evictLRU();
    }

    this.cache.set(key, {
      value,
      timestamp: Date.now(),
      hits: 0,
    });
  }

  has(key: string): boolean {
    const entry = this.cache.get(key);
    if (!entry) return false;

    // Check TTL
    if (Date.now() - entry.timestamp > this.ttl) {
      this.cache.delete(key);
      return false;
    }

    return true;
  }

  delete(key: string): boolean {
    return this.cache.delete(key);
  }

  clear(): void {
    this.cache.clear();
  }

  size(): number {
    return this.cache.size;
  }

  private evictLRU(): void {
    let oldestKey: string | null = null;
    let oldestTime = Infinity;
    let minHits = Infinity;

    for (const [key, entry] of this.cache.entries()) {
      // Evict based on age and hit count
      const score = entry.hits / (Date.now() - entry.timestamp);
      if (score < minHits / oldestTime) {
        oldestKey = key;
        oldestTime = entry.timestamp;
        minHits = entry.hits;
      }
    }

    if (oldestKey) {
      this.cache.delete(oldestKey);
    }
  }

  stats(): { size: number; maxSize: number; hitRate: number } {
    let totalHits = 0;
    let totalRequests = 0;

    for (const entry of this.cache.values()) {
      totalHits += entry.hits;
      totalRequests += entry.hits + 1;
    }

    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      hitRate: totalRequests > 0 ? totalHits / totalRequests : 0,
    };
  }
}

export const createCache = <T>(maxSize?: number, ttl?: number): LRUCache<T> => {
  return new LRUCache<T>(maxSize, ttl);
};
