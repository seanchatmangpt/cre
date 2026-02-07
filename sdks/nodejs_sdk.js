/**
 * CRE Master API - Node.js Client SDK
 *
 * Auto-generated Node.js SDK for CRE Master API v0.2.0
 *
 * The Cuneiform runtime environment (CRE) is a distributed execution environment
 * for programming languages implemented in distributed Erlang.
 *
 * @example
 * const CREClient = require('./nodejs_sdk');
 *
 * const client = new CREClient('http://localhost:4142');
 * const status = await client.getStatus();
 * console.log(`Idle workers: ${status.idle_count}`);
 */

const fetch = require('node-fetch');
const { URL, URLSearchParams } = require('url');

/**
 * Error class for API errors
 */
class APIError extends Error {
  /**
   * @param {string} error - Error code
   * @param {string} message - Error message
   * @param {number} statusCode - HTTP status code
   */
  constructor(error, message, statusCode) {
    super(message);
    this.error = error;
    this.statusCode = statusCode;
    this.name = 'APIError';
    Object.setPrototypeOf(this, APIError.prototype);
  }
}

/**
 * CRE Master API Client
 */
class CREClient {
  /**
   * Initialize the CRE client
   * @param {string} baseURL - Base URL for the API (default: http://localhost:4142)
   * @param {number} timeout - Request timeout in milliseconds (default: 30000)
   */
  constructor(baseURL = 'http://localhost:4142', timeout = 30000) {
    this.baseURL = baseURL.replace(/\/$/, '');
    this.timeout = timeout;
  }

  /**
   * Make HTTP request
   * @private
   * @param {string} method - HTTP method
   * @param {string} path - API endpoint path
   * @param {Object} options - Request options
   * @returns {Promise<Object>} Response data
   * @throws {APIError} If API returns error
   */
  async _request(method, path, options = {}) {
    const url = new URL(path.replace(/^\//, ''), this.baseURL);

    if (options.params) {
      Object.keys(options.params).forEach(key => {
        if (options.params[key] !== undefined && options.params[key] !== null) {
          url.searchParams.append(key, options.params[key]);
        }
      });
    }

    const fetchOptions = {
      method,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        ...options.headers,
      },
      timeout: this.timeout,
    };

    if (options.body) {
      fetchOptions.body = JSON.stringify(options.body);
    }

    try {
      const response = await fetch(url.toString(), fetchOptions);
      let data = {};

      const contentType = response.headers.get('content-type');
      if (contentType && contentType.includes('application/json')) {
        data = await response.json();
      }

      if (!response.ok) {
        throw new APIError(
          data.error || 'UNKNOWN_ERROR',
          data.message || `HTTP ${response.status}`,
          response.status
        );
      }

      return data;
    } catch (error) {
      if (error instanceof APIError) {
        throw error;
      }
      throw new APIError('REQUEST_ERROR', error.message, 0);
    }
  }

  /**
   * Get CRE Master status
   *
   * Retrieves the current status of the CRE Master process, including:
   * - Active worker list with status
   * - Idle worker list
   * - Pending applications queue
   * - Subscriptions information
   *
   * @returns {Promise<Object>} CRE status
   * @example
   * const status = await client.getStatus();
   * console.log(`Idle workers: ${status.idle_count}`);
   * console.log(`Busy workers: ${status.busy_count}`);
   */
  async getStatus() {
    return this._request('GET', '/status.json');
  }

  /**
   * Get execution history and cache
   *
   * Retrieves the execution history and cache map from the CRE Master.
   * Contains all memoized application-result pairs for performance optimization.
   *
   * @returns {Promise<Object>} CRE history
   * @example
   * const history = await client.getHistory();
   * console.log(`Cache hit rate: ${(history.hit_rate * 100).toFixed(1)}%`);
   * console.log(`Cached entries: ${history.cache_entries}`);
   */
  async getHistory() {
    return this._request('GET', '/history.json');
  }

  /**
   * Monitor status with polling
   *
   * Continuously polls the status endpoint and emits events
   *
   * @param {number} interval - Poll interval in milliseconds (default: 5000)
   * @param {Function} callback - Callback function called with status
   * @returns {Function} Stop function
   * @example
   * const stop = client.monitorStatus(5000, (status) => {
   *   console.log(`Workers: ${status.idle_count} idle, ${status.busy_count} busy`);
   * });
   *
   * // Later...
   * stop();
   */
  monitorStatus(interval = 5000, callback) {
    const timer = setInterval(async () => {
      try {
        const status = await this.getStatus();
        callback(status);
      } catch (error) {
        console.error('Monitor error:', error);
      }
    }, interval);

    return () => clearInterval(timer);
  }
}

/**
 * YAWL Dashboard API Client
 */
class YAWLDashboardClient {
  /**
   * Initialize the YAWL Dashboard client
   * @param {string} baseURL - Base URL for the API (default: http://localhost:8081)
   * @param {number} timeout - Request timeout in milliseconds (default: 30000)
   */
  constructor(baseURL = 'http://localhost:8081', timeout = 30000) {
    this.baseURL = baseURL.replace(/\/$/, '');
    this.timeout = timeout;
  }

  /**
   * Make HTTP request
   * @private
   */
  async _request(method, path, options = {}) {
    const url = new URL(path.replace(/^\//, ''), this.baseURL);

    if (options.params) {
      Object.keys(options.params).forEach(key => {
        if (options.params[key] !== undefined && options.params[key] !== null) {
          url.searchParams.append(key, options.params[key]);
        }
      });
    }

    const fetchOptions = {
      method,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        ...options.headers,
      },
      timeout: this.timeout,
    };

    if (options.body) {
      fetchOptions.body = JSON.stringify(options.body);
    }

    try {
      const response = await fetch(url.toString(), fetchOptions);
      let data = {};

      const contentType = response.headers.get('content-type');
      if (contentType && contentType.includes('application/json')) {
        data = await response.json();
      }

      if (!response.ok) {
        throw new APIError(
          data.error || 'UNKNOWN_ERROR',
          data.message || `HTTP ${response.status}`,
          response.status
        );
      }

      return data;
    } catch (error) {
      if (error instanceof APIError) {
        throw error;
      }
      throw new APIError('REQUEST_ERROR', error.message, 0);
    }
  }

  /**
   * Get all workflow events
   * @param {Object} options - Query options
   * @returns {Promise<Object>} Events list
   */
  async listEvents(options = {}) {
    return this._request('GET', '/api/events', { params: options });
  }

  /**
   * Get events for specific trace
   * @param {string} traceId - OpenTelemetry trace ID
   * @returns {Promise<Object>} Trace events
   */
  async getTraceEvents(traceId) {
    return this._request('GET', `/api/events/${traceId}`);
  }

  /**
   * Get all traces
   * @param {Object} options - Query options
   * @returns {Promise<Object>} Traces list
   */
  async listTraces(options = {}) {
    return this._request('GET', '/api/traces', { params: options });
  }

  /**
   * Get statistics
   * @param {Object} options - Query options
   * @returns {Promise<Object>} Statistics
   */
  async getStatistics(options = {}) {
    return this._request('GET', '/api/stats', { params: options });
  }

  /**
   * Clear all events and traces
   * @returns {Promise<Object>} Clear result
   */
  async clearData() {
    return this._request('DELETE', '/api/clear', {
      params: { confirm: 'yes' }
    });
  }
}

module.exports = CREClient;
module.exports.YAWLDashboardClient = YAWLDashboardClient;
module.exports.APIError = APIError;

// Example usage (uncomment to test)
/*
(async () => {
  try {
    const client = new CREClient();
    const status = await client.getStatus();
    console.log('CRE Status:', JSON.stringify(status, null, 2));
  } catch (error) {
    console.error('Error:', error.message);
  }
})();
*/
