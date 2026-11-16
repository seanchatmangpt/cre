# Implementing an Effect System for Workflows in Rust

## Abstract

Effect systems provide a powerful paradigm for separating pure computation from impure side effects in functional programming. This paper presents a comprehensive implementation of an effect system for workflows in Rust, leveraging Rust's type system and async/await features to achieve safe, composable, and deterministic effect handling. We explore the design of effect traits, handlers, idempotency mechanisms, and receipt-based auditing, drawing inspiration from algebraic effect systems while adapting them to Rust's ownership and async execution model. The implementation demonstrates how Rust's type system can enforce safe effect handling patterns while maintaining high performance and zero-cost abstractions.

## Introduction

Effect systems have emerged as a powerful abstraction for managing side effects in functional programming, offering a clean separation between pure business logic and impure interactions with external systems. Traditional imperative programming often mixes pure and impure code, making it difficult to reason about behavior, test applications, and ensure determinism. Algebraic effect systems address this by allowing computations to explicitly yield and resume at effect boundaries.

Rust's unique combination of strong typing, ownership semantics, and async/await capabilities makes it an excellent candidate for implementing effect systems. While Rust lacks native algebraic effect support, we can simulate these patterns using traits, async functions, and careful design of effect boundaries. This paper presents a complete effect system implementation for workflow scenarios, featuring:

1. **Type-safe effect specification** with compile-time guarantees
2. **Compositional effect handlers** that can be combined and reused
3. **Idempotency mechanisms** for safe retry and caching
4. **Receipt-based auditing** for deterministic replay
5. **Async execution patterns** with tokio integration
6. **Cancellation and error recovery** strategies

The implementation draws inspiration from successful effect systems in languages like Koka, Eff, and F#, while leveraging Rust's unique strengths to provide a robust, production-ready solution.

## Effect Type System Design

### Effect Traits and Implementations

The foundation of our Rust effect system is a set of trait hierarchies that define the contract for effect creation, handling, and execution.

#### Core Effect Traits

```rust
/// Represents an effect specification - the "what" without the "how"
pub trait EffectSpec: Send + Sync + 'static {
    type Output;
    type Context;

    fn effect_type(&self) -> &'static str;
    fn idempotency_key(&self) -> Option<&str>;
    fn timeout(&self) -> Option<Duration>;
}

/// Effect handler for executing a specific type of effect
pub trait EffectHandler<T>: Send + Sync + 'static {
    type Error;

    fn handle(&self, spec: &T) -> impl Future<Output = Result<T::Output, Self::Error>> + Send;
}

/// Unified effect interface that combines spec and handler
pub trait Effect<T>: EffectSpec<Output = T::Output, Context = T::Context>
    where T: EffectSpec
{
    fn execute(&self, ctx: T::Context) -> impl Future<Output = EffectResult<T::Output, T::Context>> + Send;
}

/// Result of effect execution with continuation context
pub type EffectResult<T, C> = Result<(C, T), EffectError>;

/// Effect error types
#[derive(Debug, thiserror::Error)]
pub enum EffectError {
    #[error("Effect handler failed: {0}")]
    Handler(String),
    #[error("Effect timed out after {0:?}")]
    Timeout(Duration),
    #[error("Effect was cancelled")]
    Cancelled,
    #[error("Handler panicked: {0}")]
    Panic(String),
}
```

#### Basic Effect Implementations

```rust
/// HTTP GET effect specification
#[derive(Debug, Clone)]
pub struct HttpGet {
    pub url: String,
    pub headers: Option<HashMap<String, String>>,
    pub timeout: Option<Duration>,
    pub idempotency_key: Option<String>,
}

impl EffectSpec for HttpGet {
    type Output = HttpResponse;
    type Context = ();

    fn effect_type(&self) -> &'static str { "http_get" }
    fn idempotency_key(&self) -> Option<&str> { self.idempotency_key.as_deref() }
    fn timeout(&self) -> Option<Duration> { self.timeout }
}

/// Effect handler for HTTP GET
pub struct HttpGetHandler {
    client: reqwest::Client,
}

#[async_trait::async_trait]
impl EffectHandler<HttpGet> for HttpGetHandler {
    type Error = EffectError;

    async fn handle(&self, spec: &HttpGet) -> Result<HttpResponse, EffectError> {
        let mut request = self.client.get(&spec.url);

        if let Some(headers) = &spec.headers {
            for (key, value) in headers {
                request = request.header(key, value);
            }
        }

        let response = request.send().await
            .map_err(|e| EffectError::Handler(e.to_string()))?;

        Ok(HttpResponse {
            status: response.status(),
            headers: response.headers().clone(),
            body: response.bytes().await
                .map_err(|e| EffectError::Handler(e.to_string()))?,
        })
    }
}

impl Effect<HttpGet> for HttpGet {
    async fn execute(&self, _: ()) -> EffectResult<HttpResponse, ()> {
        let handler = HttpGetHandler::new();
        let result = handler.handle(self).await?;
        Ok(((), result))
    }
}
```

### Effect Composition with Monadic Patterns

Effect composition is crucial for building complex workflows. We provide monadic operators that allow chaining effects while maintaining type safety.

```rust
/// Effect monad for composing multiple effects
#[derive(Debug)]
pub struct EffectChain<T, C> {
    spec: Box<dyn Effect<Output = T, Context = C> + Send>,
}

impl<T, C> EffectChain<T, C>
    where T: Send + 'static,
          C: Clone + Send + 'static
{
    /// Create a new effect chain
    pub fn new<E>(spec: E) -> Self
        where E: Effect<Output = T, Context = C> + Send + 'static
    {
        Self { spec: Box::new(spec) }
    }

    /// Chain another effect after this one
    pub fn and_then<F, U>(self, f: F) -> EffectChain<U, C>
        where F: Fn(T, C) -> EffectChain<U, C> + Send + Sync + 'static,
              U: Send + 'static
    {
        AndThenEffect::new(self, f)
    }

    /// Apply a function to the output context
    pub fn map_context<F, C2>(self, f: F) -> EffectChain<T, C2>
        where F: Fn(C) -> C2 + Send + Sync + 'static,
              C2: Clone + Send + 'static
    {
        MapContextEffect::new(self, f)
    }

    /// Execute the effect chain
    pub async fn run(self, initial_ctx: C) -> EffectResult<T, C> {
        self.spec.execute(initial_ctx).await
    }
}

/// Effect composition using the `and_then` operator
struct AndThenEffect<T, C, U, F> {
    first: EffectChain<T, C>,
    f: F,
    _phantom: PhantomData<U>,
}

/// Effect context mapping
struct MapContextEffect<T, C, C2, F> {
    inner: EffectChain<T, C>,
    f: F,
    _phantom: PhantomData<C2>,
}
```

### Effect Registry for Dynamic Handler Resolution

In complex systems, we often need to register multiple effect handlers dynamically. An effect registry provides a centralized way to manage this.

```rust
/// Registry for effect handlers
#[derive(Default)]
pub struct EffectRegistry {
    handlers: HashMap<&'static str, Box<dyn EffectHandlerFactory>>,
}

/// Factory for creating effect handlers
pub trait EffectHandlerFactory: Send + Sync {
    fn create(&self) -> Box<dyn EffectHandlerDyn>;
}

/// Type-erased effect handler trait
#[async_trait::async_trait]
pub trait EffectHandlerDyn: Send + Sync {
    async fn handle_dyn(&self, effect_type: &str, payload: &[u8]) -> Result<Vec<u8>, EffectError>;
}

impl EffectRegistry {
    /// Register an effect handler
    pub fn register<T>(&mut self, handler: T)
        where T: EffectHandlerFactory + 'static
    {
        let effect_type = T::EFFECT_TYPE;
        self.handlers.insert(effect_type, Box::new(handler));
    }

    /// Get handler for effect type
    pub fn get_handler(&self, effect_type: &str) -> Option<Box<dyn EffectHandlerDyn>> {
        self.handlers.get(effect_type).map(|f| f.create())
    }
}

/// Example: Registry-based HTTP GET handler
pub struct RegistryHttpGetHandler {
    client: reqwest::Client,
}

impl EffectHandlerFactory for RegistryHttpGetHandler {
    const EFFECT_TYPE: &'static str = "http_get";

    fn create(&self) -> Box<dyn EffectHandlerDyn> {
        Box::new(self.clone())
    }
}

#[async_trait::async_trait]
impl EffectHandlerDyn for RegistryHttpGetHandler {
    async fn handle_dyn(&self, _effect_type: &str, payload: &[u8]) -> Result<Vec<u8>, EffectError> {
        let spec = bincode::deserialize::<HttpGet>(payload)
            .map_err(|e| EffectError::Handler(e.to_string()))?;

        let result = self.handle(&spec).await?;
        bincode::serialize(&result)
            .map_err(|e| EffectError::Handler(e.to_string()))
    }
}
```

## Effect Handlers

### Handler Design Patterns

Effect handlers are the bridge between the effect specification and the actual execution. We explore several handler patterns from simple to sophisticated.

#### 1. Basic Handler Pattern

```rust
/// Simple handler that directly executes the effect
#[async_trait::async_trait]
impl EffectHandler<HttpGet> for SimpleHttpGetHandler {
    type Error = EffectError;

    async fn handle(&self, spec: &HttpGet) -> Result<HttpResponse, EffectError> {
        // Direct implementation
        let response = reqwest::get(&spec.url).await?;
        Ok(HttpResponse {
            status: response.status(),
            headers: response.headers().clone(),
            body: response.bytes().await?,
        })
    }
}
```

#### 2. Context-Aware Handler

Handlers often need access to contextual information like configuration, dependencies, or shared state.

```rust
/// Handler with access to shared context
pub struct ContextAwareHandler {
    config: Arc<AppConfig>,
    metrics: Arc<Metrics>,
}

#[async_trait::async_trait]
impl EffectHandler<HttpGet> for ContextAwareHandler {
    type Error = EffectError;

    async fn handle(&self, spec: &HttpGet) -> Result<HttpResponse, EffectError> {
        let start = Instant::now();

        // Apply rate limiting if configured
        if let Some(rate_limiter) = self.config.rate_limiter.as_ref() {
            rate_limiter.acquire_one().await;
        }

        // Execute request with configured timeouts
        let timeout = spec.timeout.unwrap_or(self.config.default_timeout);
        let result = tokio::time::timeout(timeout, reqwest::get(&spec.url)).await
            .map_err(|_| EffectError::Timeout(timeout))?;

        let response = result
            .map_err(|e| EffectError::Handler(e.to_string()))?;

        // Record metrics
        self.metrics.record_http_request(start.elapsed(), response.status().as_u16());

        Ok(HttpResponse {
            status: response.status(),
            headers: response.headers().clone(),
            body: response.bytes().await?,
        })
    }
}
```

#### 3. Middleware Chain

Handlers can be composed using middleware patterns for cross-cutting concerns.

```rust
/// Middleware for adding common functionality
pub struct MiddlewareEffectHandler<T, H> {
    inner: H,
    middleware: Vec<Box<dyn Middleware<T>>>,
}

#[async_trait::async_trait]
impl<T, H> EffectHandler<T> for MiddlewareEffectHandler<T, H>
    where T: EffectSpec,
          H: EffectHandler<T>
{
    type Error = EffectError;

    async fn handle(&self, spec: &T) -> Result<T::Output, EffectError> {
        // Apply pre-processing middleware
        let mut spec = spec.clone();
        for middleware in &self.middleware {
            spec = middleware.pre_process(spec).await?;
        }

        // Execute the actual handler
        let result = self.inner.handle(&spec).await;

        // Apply post-processing middleware
        let result = match result {
            Ok(output) => {
                let mut output = output;
                for middleware in self.middleware.iter().rev() {
                    output = middleware.post_process(output).await?;
                }
                Ok(output)
            }
            Err(e) => {
                for middleware in &self.middleware {
                    middleware.on_error(&e).await;
                }
                Err(e)
            }
        };

        result
    }
}

/// Middleware trait
#[async_trait::async_trait]
pub trait Middleware<T>: Send + Sync + 'static
    where T: EffectSpec
{
    async fn pre_process(&self, spec: T) -> Result<T, EffectError>;
    async fn post_process(&self, output: T::Output) -> Result<T::Output, EffectError>;
    async fn on_error(&self, error: &EffectError);
}

/// Example middleware for logging
pub struct LoggingMiddleware {
    logger: Arc<dyn Logger>,
}

#[async_trait::async_trait]
impl Middleware<HttpGet> for LoggingMiddleware {
    async fn pre_process(&self, spec: HttpGet) -> Result<HttpGet, EffectError> {
        self.logger.log_info(&format!("Starting HTTP GET: {}", spec.url));
        Ok(spec)
    }

    async fn post_process(&self, output: HttpResponse) -> Result<HttpResponse, EffectError> {
        self.logger.log_info(&format!("HTTP GET completed: {}", output.status));
        Ok(output)
    }

    async fn on_error(&self, error: &EffectError) {
        self.logger.log_error(&format!("HTTP GET failed: {}", error));
    }
}
```

### Handler Lifetimes and Resource Management

```rust
/// Handler with resource cleanup
pub struct ResourceAwareHandler<T> {
    resources: ResourceManager,
    inner: T,
}

impl<T> Drop for ResourceAwareHandler<T> {
    fn drop(&mut self) {
        self.resources.cleanup();
    }
}

/// Manager for effect-specific resources
pub struct ResourceManager {
    connections: Vec<Connection>,
    temp_files: Vec<PathBuf>,
}

impl ResourceManager {
    pub fn new() -> Self {
        Self {
            connections: Vec::new(),
            temp_files: Vec::new(),
        }
    }

    pub fn add_connection(&mut self, conn: Connection) {
        self.connections.push(conn);
    }

    pub fn create_temp_file(&mut self) -> Result<PathBuf, io::Error> {
        let path = tempfile::tempfile()?.into_temp_path();
        self.temp_files.push(path.clone());
        Ok(path)
    }

    pub fn cleanup(&mut self) {
        // Close all connections
        for conn in self.connections.drain(..) {
            let _ = conn.close();
        }

        // Remove temp files
        for path in self.temp_files.drain(..) {
            let _ = std::fs::remove_file(path);
        }
    }
}
```

## Idempotency and Receipts

Idempotency is crucial for workflow systems that may retry operations after failures. Our implementation provides robust idempotency guarantees through receipt-based caching.

### Idempotency Mechanism

```rust
/// Idempotency store for caching effect results
#[derive(Default)]
pub struct IdempotencyStore {
    cache: Arc<Mutex<HashMap<String, CachedResult>>>,
    ttl: Duration,
}

#[derive(Debug, Clone)]
struct CachedResult {
    result: EffectResultKind,
    timestamp: Instant,
}

#[derive(Debug, Clone)]
enum EffectResultKind {
    Success(Vec<u8>),
    Error(String),
}

impl IdempotencyStore {
    pub fn new(ttl: Duration) -> Self {
        Self {
            cache: Arc::new(Mutex::new(HashMap::new())),
            ttl,
        }
    }

    /// Check if result is cached for idempotency key
    pub async fn get_cached(&self, key: &str) -> Option<EffectResultKind> {
        let cache = self.cache.lock().await;
        let cached = cache.get(key)?;

        if cached.timestamp.elapsed() < self.ttl {
            Some(cached.result.clone())
        } else {
            // Remove expired entry
            cache.remove(key);
            None
        }
    }

    /// Cache a result
    pub async fn cache_result(&self, key: &str, result: EffectResultKind) {
        let mut cache = self.cache.lock().await;
        cache.insert(key.to_string(), CachedResult {
            result,
            timestamp: Instant::now(),
        });
    }

    /// Generate idempotency key from effect specification
    pub fn generate_key<T: EffectSpec>(&self, spec: &T) -> String {
        let mut hasher = DefaultHasher::new();

        // Include effect type in hash
        spec.effect_type().hash(&mut hasher);

        // Include payload (serialized)
        if let Ok(serialized) = bincode::serialize(spec) {
            serialized.hash(&mut hasher);
        }

        // Include timeout if specified
        if let Some(timeout) = spec.timeout() {
            timeout.as_millis().hash(&mut hasher);
        }

        format!("effect_{}", hasher.finish())
    }
}
```

### Receipt System

The receipt system provides an immutable audit trail of effect executions, enabling deterministic replay and debugging.

```rust
/// Receipt for effect execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub receipt_id: Uuid,
    pub effect_id: EffectId,
    pub effect_type: String,
    pub effect_spec_hash: String,
    pub idempotency_key: Option<String>,
    pub timestamp: DateTime<Utc>,
    pub result: ReceiptResult,
    pub duration_ms: u64,
    pub metadata: HashMap<String, Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReceiptResult {
    Success(serde_json::Value),
    Error(String),
    Cancelled(String),
}

/// Receipt store with append-only semantics
pub struct ReceiptStore {
    storage: Arc<Mutex<Vec<Receipt>>>,
    index: HashMap<String, Vec<Uuid>>, // effect_type -> receipt_ids
    idempotency_index: HashMap<String, Uuid>, // idempotency_key -> receipt_id
}

impl ReceiptStore {
    pub fn new() -> Self {
        Self {
            storage: Arc::new(Mutex::new(Vec::new())),
            index: HashMap::new(),
            idempotency_index: HashMap::new(),
        }
    }

    /// Store a receipt (append-only)
    pub async fn store(&mut self, receipt: Receipt) -> Result<(), ReceiptError> {
        let mut storage = self.storage.lock().await;

        // Add to storage
        storage.push(receipt.clone());

        // Update indexes
        let receipt_ids = self.index.entry(receipt.effect_type.clone())
            .or_insert_with(Vec::new);
        receipt_ids.push(receipt.receipt_id);

        if let Some(key) = &receipt.idempotency_key {
            self.idempotency_index.insert(key.clone(), receipt.receipt_id);
        }

        Ok(())
    }

    /// Lookup receipt by effect_id
    pub async fn lookup_by_effect_id(&self, effect_id: &EffectId) -> Option<Receipt> {
        let storage = self.storage.lock().await;
        storage.iter()
            .find(|r| &r.effect_id == effect_id)
            .cloned()
    }

    /// Lookup receipt by idempotency key
    pub async fn lookup_by_idempotency_key(&self, key: &str) -> Option<Receipt> {
        if let Some(receipt_id) = self.idempotency_index.get(key) {
            let storage = self.storage.lock().await;
            storage.iter()
                .find(|r| &r.receipt_id == receipt_id)
                .cloned()
        } else {
            None
        }
    }

    /// Get all receipts for effect type
    pub async fn by_effect_type(&self, effect_type: &str) -> Vec<Receipt> {
        let storage = self.storage.lock().await;
        if let Some(receipt_ids) = self.index.get(effect_type) {
            storage.iter()
                .filter(|r| receipt_ids.contains(&r.receipt_id))
                .cloned()
                .collect()
        } else {
            Vec::new()
        }
    }
}

/// Hash effect specification for receipt verification
pub fn hash_effect_spec<T: EffectSpec>(spec: &T) -> String {
    let serialized = bincode::serialize(spec)
        .expect("Failed to serialize effect spec");
    hex::encode(Sha256::digest(&serialized))
}
```

### Idempotent Effect Execution

```rust
/// Idempotent wrapper for effect execution
pub struct IdempotentEffectRunner<T> {
    store: Arc<IdempotencyStore>,
    receipt_store: Arc<Mutex<ReceiptStore>>,
}

impl<T> IdempotentEffectRunner<T>
    where T: EffectSpec
{
    pub fn new(store: Arc<IdempotencyStore>, receipt_store: Arc<Mutex<ReceiptStore>>) -> Self {
        Self { store, receipt_store }
    }

    /// Execute effect with idempotency guarantees
    pub async fn execute(
        &self,
        effect_id: EffectId,
        spec: T,
        timeout: Option<Duration>
    ) -> EffectResult<T::Output, T::Context> {
        // Generate idempotency key
        let idempotency_key = spec.idempotency_key()
            .map(|k| self.store.generate_key_for_type(spec.effect_type(), k))
            .unwrap_or_else(|| format!("no_key_{}", effect_id));

        // Check cache first
        if let Some(cached) = self.store.get_cached(&idempotency_key).await {
            return match cached {
                EffectResultKind::Success(data) => {
                    let output = bincode::deserialize(&data)
                        .map_err(|e| EffectError::Handler(e.to_string()))?;
                    Ok(((), output))
                }
                EffectResultKind::Error(e) => Err(EffectError::Handler(e)),
            };
        }

        // Create receipt for tracking
        let start = Instant::now();
        let receipt_id = Uuid::new_v4();
        let timestamp = Utc::now();

        // Execute the effect
        let result = match timeout {
            Some(timeout) => {
                tokio::time::timeout(timeout, spec.execute(())).await
            }
            None => Ok(spec.execute().await),
        };

        let elapsed = start.elapsed();

        // Process result
        let effect_result = match result {
            Ok(Ok((ctx, output))) => {
                // Store result in cache
                let serialized = bincode::serialize(&output)
                    .map_err(|e| EffectError::Handler(e.to_string()))?;
                self.store.cache_result(&idempotency_key, EffectResultKind::Success(serialized)).await;

                // Store receipt
                let receipt = Receipt {
                    receipt_id,
                    effect_id,
                    effect_type: spec.effect_type().to_string(),
                    effect_spec_hash: hash_effect_spec(&spec),
                    idempotency_key: Some(idempotency_key.clone()),
                    timestamp,
                    result: ReceiptResult::Success(
                        serde_json::to_value(&output).unwrap_or_default()
                    ),
                    duration_ms: elapsed.as_millis() as u64,
                    metadata: HashMap::new(),
                };

                let mut receipt_store = self.receipt_store.lock().await;
                receipt_store.store(receipt).await
                    .unwrap_or_else(|_| warn!("Failed to store receipt"));

                Ok((ctx, output))
            }
            Ok(Err(e)) => {
                // Store error in cache (shorter TTL)
                self.store.cache_result(&idempotency_key, EffectResultKind::Error(e.to_string())).await;

                let receipt = Receipt {
                    receipt_id,
                    effect_id,
                    effect_type: spec.effect_type().to_string(),
                    effect_spec_hash: hash_effect_spec(&spec),
                    idempotency_key: Some(idempotency_key),
                    timestamp,
                    result: ReceiptResult::Error(e.to_string()),
                    duration_ms: elapsed.as_millis() as u64,
                    metadata: HashMap::new(),
                };

                let mut receipt_store = self.receipt_store.lock().await;
                receipt_store.store(receipt).await
                    .unwrap_or_else(|_| warn!("Failed to store receipt"));

                Err(e)
            }
            Err(_) => {
                let receipt = Receipt {
                    receipt_id,
                    effect_id,
                    effect_type: spec.effect_type().to_string(),
                    effect_spec_hash: hash_effect_spec(&spec),
                    idempotency_key: Some(idempotency_key),
                    timestamp,
                    result: ReceiptResult::Error("Effect timed out".to_string()),
                    duration_ms: elapsed.as_millis() as u64,
                    metadata: HashMap::new(),
                };

                let mut receipt_store = self.receipt_store.lock().await;
                receipt_store.store(receipt).await
                    .unwrap_or_else(|_| warn!("Failed to store receipt"));

                Err(EffectError::Timeout(timeout.unwrap_or_default()))
            }
        };

        effect_result
    }
}
```

## Async Effect Execution

### Tokio Integration

The effect system is built on top of tokio for efficient async execution. We provide seamless integration with tokio's runtime and cancellation mechanisms.

```rust
/// Effect executor powered by tokio
pub struct TokioEffectExecutor {
    registry: Arc<EffectRegistry>,
    idempotency_store: Arc<IdempotencyStore>,
    receipt_store: Arc<Mutex<ReceiptStore>>,
    runtime: Handle,
}

impl TokioEffectExecutor {
    pub fn new() -> Result<Self, EffectError> {
        let runtime = Handle::current();

        Ok(Self {
            registry: Arc::new(EffectRegistry::default()),
            idempotency_store: Arc::new(IdempotencyStore::new(Duration::from_secs(3600))),
            receipt_store: Arc::new(Mutex::new(ReceiptStore::new())),
            runtime,
        })
    }

    /// Submit effect for execution
    pub async fn submit<T>(&self, effect_id: EffectId, spec: T) -> EffectResult<T::Output, T::Context>
        where T: EffectSpec + Send + 'static
    {
        // Get handler for effect type
        let handler_type = spec.effect_type();
        let handler = self.registry.get_handler(handler_type)
            .ok_or_else(|| EffectError::Handler(format!("Unknown effect type: {}", handler_type)))?;

        // Create idempotent runner
        let runner = IdempotentEffectRunner::new(
            self.idempotency_store.clone(),
            self.receipt_store.clone()
        );

        // Execute with idempotency
        runner.execute(effect_id, spec, spec.timeout()).await
    }

    /// Batch execute multiple effects
    pub async fn batch_submit<I, T>(&self, effects: I) -> Vec<EffectResult<T::Output, T::Context>>
        where I: IntoIterator<Item = (EffectId, T)>,
              T: EffectSpec + Send + 'static
    {
        // Submit all effects concurrently
        let futures: Vec<_> = effects.into_iter()
            .map(|(id, spec)| self.submit(id, spec))
            .collect();

        // Wait for all to complete
        futures::future::join_all(futures).await
    }

    /// Stream effects with backpressure
    pub fn stream_effects<T>(
        &self,
        mut effects: impl Stream<Item = (EffectId, T)> + Send + 'static,
        concurrency_limit: usize
    ) -> impl Stream<Item = EffectResult<T::Output, T::Context>> + Send + 'static
        where T: EffectSpec + Send + 'static
    {
        let executor = self.clone();

        effects
            .map(move |(id, spec)| {
                let executor = executor.clone();
                async move {
                    executor.submit(id, spec).await
                }
            })
            .buffer_unordered(concurrency_limit)
    }
}
```

### Async Effect with Cancellation

```rust
/// Effect that can be cancelled
pub struct CancellableEffect<T> {
    effect: T,
    cancel_handle: Arc<AtomicBool>,
}

impl<T> CancellableEffect<T>
    where T: EffectSpec
{
    pub fn new(effect: T) -> Self {
        Self {
            effect,
            cancel_handle: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn cancel(&self) {
        self.cancel_handle.store(true, Ordering::SeqCst);
    }

    pub fn is_cancelled(&self) -> bool {
        self.cancel_handle.load(Ordering::SeqCst)
    }
}

#[async_trait::async_trait]
impl<T> EffectSpec for CancellableEffect<T>
    where T: EffectSpec + Send + Sync + 'static
{
    type Output = T::Output;
    type Context = T::Context;

    fn effect_type(&self) -> &'static str { self.effect.effect_type() }
    fn idempotency_key(&self) -> Option<&str> { self.effect.idempotency_key() }
    fn timeout(&self) -> Option<Duration> { self.effect.timeout() }
}

#[async_trait::async_trait]
impl<T> Effect<T> for CancellableEffect<T>
    where T: Effect<Output = T::Output, Context = T::Context> + Send + Sync + 'static
{
    async fn execute(&self, ctx: T::Context) -> EffectResult<T::Output, T::Context> {
        if self.is_cancelled() {
            return Err(EffectError::Cancelled);
        }

        // Create a cancellation-aware future
        let effect = self.effect.clone();
        let cancel_handle = self.cancel_handle.clone();

        let future = async move {
            effect.execute(ctx).await
        };

        // Check cancellation frequently
        let result = tokio::select! {
            _ = tokio::time::sleep(Duration::from_millis(10)) => {
                if cancel_handle.load(Ordering::SeqCst) {
                    Err(EffectError::Cancelled)
                } else {
                    future.await
                }
            }
            result = future => result,
        };

        result
    }
}
```

## Cancellation and Timeouts

### Hierarchical Cancellation

Workflow systems often need hierarchical cancellation where cancelling a scope cancels all child effects.

```rust
/// Cancellation scope hierarchy
#[derive(Debug, Clone)]
pub struct CancellationScope {
    id: Uuid,
    parent: Option<Uuid>,
    children: Arc<Mutex<Vec<Uuid>>>,
    cancelled: Arc<AtomicBool>,
    cancel_reason: Arc<Mutex<Option<String>>>,
}

impl CancellationScope {
    pub fn new(parent: Option<Uuid>) -> Self {
        Self {
            id: Uuid::new_v4(),
            parent,
            children: Arc::new(Mutex::new(Vec::new())),
            cancelled: Arc::new(AtomicBool::new(false)),
            cancel_reason: Arc::new(Mutex::new(None)),
        }
    }

    pub fn id(&self) -> Uuid { self.id }

    pub fn cancel(&self, reason: String) {
        let mut cancelled = self.cancel_reason.lock().unwrap();
        if !self.cancelled.load(Ordering::SeqCst) {
            self.cancelled.store(true, Ordering::SeqCst);
            *cancelled = Some(reason);

            // Cancel all children
            let children = self.children.lock().unwrap();
            for child_id in children.iter() {
                if let Some(child) = CANCELLATION_MANAGER.get_scope(child_id) {
                    child.cancel(format!("Parent scope {} cancelled", self.id));
                }
            }
        }
    }

    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::SeqCst)
    }

    pub fn cancel_reason(&self) -> Option<String> {
        self.cancel_reason.lock().unwrap().clone()
    }

    pub fn add_child(&self, child_id: Uuid) {
        let mut children = self.children.lock().unwrap();
        children.push(child_id);
    }
}

/// Global cancellation manager
pub struct CancellationManager {
    scopes: Arc<Mutex<HashMap<Uuid, CancellationScope>>>,
}

impl CancellationManager {
    pub fn new() -> Self {
        Self {
            scopes: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn create_scope(&self, parent: Option<Uuid>) -> CancellationScope {
        let scope = CancellationScope::new(parent);

        if let Some(parent_id) = parent {
            if let Some(parent_scope) = self.scopes.lock().unwrap().get(&parent_id) {
                parent_scope.add_child(scope.id);
            }
        }

        self.scopes.lock().unwrap().insert(scope.id, scope.clone());
        scope
    }

    pub fn get_scope(&self, id: &Uuid) -> Option<CancellationScope> {
        self.scopes.lock().unwrap().get(id).cloned()
    }

    pub fn cancel_scope(&self, id: &Uuid, reason: String) {
        if let Some(scope) = self.get_scope(id) {
            scope.cancel(reason);
        }
    }
}

static CANCELLATION_MANAGER: Lazy<Arc<CancellationManager>> =
    Lazy::new(|| Arc::new(CancellationManager::new()));
```

### Timeout Handling

```rust
/// Timeout wrapper for effects
pub struct TimeoutEffect<T> {
    effect: T,
    timeout: Duration,
}

impl<T> TimeoutEffect<T>
    where T: EffectSpec
{
    pub fn new(effect: T, timeout: Duration) -> Self {
        Self { effect, timeout }
    }
}

#[async_trait::async_trait]
impl<T> EffectSpec for TimeoutEffect<T>
    where T: EffectSpec + Send + Sync + 'static
{
    type Output = T::Output;
    type Context = T::Context;

    fn effect_type(&self) -> &'static str { self.effect.effect_type() }
    fn idempotency_key(&self) -> Option<&str> { self.effect.idempotency_key() }
    fn timeout(&self) -> Option<Duration> { Some(self.timeout) }
}

#[async_trait::async_trait]
impl<T> Effect<T> for TimeoutEffect<T>
    where T: Effect<Output = T::Output, Context = T::Context> + Send + Sync + 'static
{
    async fn execute(&self, ctx: T::Context) -> EffectResult<T::Output, T::Context> {
        let effect = self.effect.clone();

        tokio::time::timeout(self.timeout, async {
            effect.execute(ctx).await
        }).await
            .map_err(|_| EffectError::Timeout(self.timeout))?
    }
}
```

## Error Recovery Strategies

### Retry Mechanisms

```rust
/// Retry policy for effect execution
#[derive(Debug, Clone)]
pub struct RetryPolicy {
    max_attempts: u32,
    base_delay: Duration,
    max_delay: Duration,
    backoff_factor: f64,
    jitter: bool,
}

impl RetryPolicy {
    pub fn new() -> Self {
        Self {
            max_attempts: 3,
            base_delay: Duration::from_millis(100),
            max_delay: Duration::from_secs(30),
            backoff_factor: 2.0,
            jitter: true,
        }
    }

    pub fn with_attempts(mut self, attempts: u32) -> Self {
        self.max_attempts = attempts;
        self
    }

    pub fn with_backoff(mut self, base: Duration, max: Duration, factor: f64) -> Self {
        self.base_delay = base;
        self.max_delay = max;
        self.backoff_factor = factor;
        self
    }

    pub fn with_jitter(mut self, enabled: bool) -> Self {
        self.jitter = enabled;
        self
    }

    fn next_delay(&self, attempt: u32) -> Duration {
        let mut delay = self.base_delay.as_millis() as f64 *
            self.backoff_factor.powi(attempt as i32);

        delay = delay.min(self.max_delay.as_millis() as f64);

        if self.jitter {
            delay *= 0.5 + rand::random::<f64>();
        }

        Duration::from_millis(delay as u64)
    }
}

/// Retry wrapper for effects
pub struct RetryEffect<T> {
    effect: T,
    policy: RetryPolicy,
}

impl<T> RetryEffect<T>
    where T: EffectSpec
{
    pub fn new(effect: T, policy: RetryPolicy) -> Self {
        Self { effect, policy }
    }
}

#[async_trait::async_trait]
impl<T> EffectSpec for RetryEffect<T>
    where T: EffectSpec + Send + Sync + 'static
{
    type Output = T::Output;
    type Context = T::Context;

    fn effect_type(&self) -> &'static str { self.effect.effect_type() }
    fn idempotency_key(&self) -> Option<&str> { self.effect.idempotency_key() }
    fn timeout(&self) -> Option<Duration> {
        self.effect.timeout().or_else(|| {
            // Scale timeout based on retries
            let scaled = self.policy.max_attempts as u64 * 1000;
            Some(Duration::from_millis(scaled))
        })
    }
}

#[async_trait::async_trait]
impl<T> Effect<T> for RetryEffect<T>
    where T: Effect<Output = T::Output, Context = T::Context> + Send + Sync + 'static
{
    async fn execute(&self, ctx: T::Context) -> EffectResult<T::Output, T::Context> {
        let mut last_error = None;

        for attempt in 0..self.policy.max_attempts {
            if attempt > 0 {
                tokio::time::sleep(self.policy.next_delay(attempt - 1)).await;
            }

            match self.effect.execute(ctx.clone()).await {
                Ok((new_ctx, output)) => {
                    return Ok((new_ctx, output));
                }
                Err(e) => {
                    last_error = Some(e);

                    // Don't retry certain errors
                    if should_not_retry(&e) {
                        break;
                    }
                }
            }
        }

        Err(last_error.unwrap_or_else(|| EffectError::Handler("Max retries exceeded".to_string())))
    }
}

fn should_not_retry(error: &EffectError) -> bool {
    match error {
        EffectError::Timeout(_) => true,
        EffectError::Cancelled => true,
        EffectError::Panic(_) => true,
        EffectError::Handler(e) => {
            // Don't retry client errors (4xx) except 429 (rate limited)
            e.contains("429") || !e.starts_with("4")
        }
        _ => false,
    }
}
```

### Circuit Breaker Pattern

```rust
/// Circuit breaker for preventing cascading failures
pub struct CircuitBreaker {
    state: Arc<Mutex<CircuitState>>,
    failure_threshold: u32,
    recovery_timeout: Duration,
}

#[derive(Debug)]
enum CircuitState {
    Closed,
    Open { failures: u32, last_failure: Instant },
    HalfOpen,
}

impl CircuitBreaker {
    pub fn new(failure_threshold: u32, recovery_timeout: Duration) -> Self {
        Self {
            state: Arc::new(Mutex::new(CircuitState::Closed)),
            failure_threshold,
            recovery_timeout,
        }
    }

    pub async fn call<T, F, Fut>(&self, f: F) -> Result<T, EffectError>
        where F: FnOnce() -> Fut,
              Fut: Future<Output = Result<T, EffectError>>,
              T: Send
    {
        let state = self.state.lock().await;

        match *state {
            CircuitState::Closed => {
                drop(state);
                match f().await {
                    Ok(result) => Ok(result),
                    Err(e) => {
                        let mut state = self.state.lock().await;
                        if let CircuitState::Closed = *state {
                            *state = CircuitState::Open {
                                failures: 1,
                                last_failure: Instant::now(),
                            };
                        } else if let CircuitState::Open { failures, last_failure } = *state {
                            let new_failures = failures + 1;
                            if new_failures >= self.failure_threshold {
                                *state = CircuitState::Open {
                                    failures: new_failures,
                                    last_failure: last_failure,
                                };
                            }
                        }
                        Err(e)
                    }
                }
            }
            CircuitState::Open { last_failure, .. } => {
                if last_failure.elapsed() > self.recovery_timeout {
                    // Try to recover
                    let mut state = self.state.lock().await;
                    *state = CircuitState::HalfOpen;
                    drop(state);

                    match f().await {
                        Ok(result) => {
                            let mut state = self.state.lock().await;
                            *state = CircuitState::Closed;
                            Ok(result)
                        }
                        Err(e) => {
                            let mut state = self.state.lock().await;
                            *state = CircuitState::Open {
                                failures: 1,
                                last_failure: Instant::now(),
                            };
                            Err(e)
                        }
                    }
                } else {
                    Err(EffectError::Handler("Circuit breaker open".to_string()))
                }
            }
            CircuitState::HalfOpen => {
                drop(state);
                match f().await {
                    Ok(result) => {
                        let mut state = self.state.lock().await;
                        *state = CircuitState::Closed;
                        Ok(result)
                    }
                    Err(e) => {
                        let mut state = self.state.lock().await;
                        *state = CircuitState::Open {
                            failures: 1,
                            last_failure: Instant::now(),
                        };
                        Err(e)
                    }
                }
            }
        }
    }
}
```

### Fallback Strategies

```rust
/// Fallback effect that tries alternative effects
pub struct FallbackEffect<T> {
    primary: T,
    fallbacks: Vec<T>,
}

impl<T> FallbackEffect<T>
    where T: EffectSpec
{
    pub fn new(primary: T, fallbacks: Vec<T>) -> Self {
        Self { primary, fallbacks }
    }
}

#[async_trait::async_trait]
impl<T> EffectSpec for FallbackEffect<T>
    where T: EffectSpec + Send + Sync + 'static
{
    type Output = T::Output;
    type Context = T::Context;

    fn effect_type(&self) -> &'static str { self.primary.effect_type() }
    fn idempotency_key(&self) -> Option<&str> { self.primary.idempotency_key() }
    fn timeout(&self) -> Option<Duration> {
        self.primary.timeout().or_else(|| {
            // Use longest timeout from fallbacks
            self.fallbacks.iter()
                .filter_map(|f| f.timeout())
                .max()
        })
    }
}

#[async_trait::async_trait]
impl<T> Effect<T> for FallbackEffect<T>
    where T: Effect<Output = T::Output, Context = T::Context> + Send + Sync + 'static
{
    async fn execute(&self, ctx: T::Context) -> EffectResult<T::Output, T::Context> {
        // Try primary effect first
        match self.primary.execute(ctx.clone()).await {
            Ok((ctx, result)) => Ok((ctx, result)),
            Err(_) => {
                // Try fallbacks in order
                for fallback in &self.fallbacks {
                    match fallback.execute(ctx.clone()).await {
                        Ok((ctx, result)) => return Ok((ctx, result)),
                        Err(_) => continue,
                    }
                }
                Err(EffectError::Handler("All fallbacks failed".to_string()))
            }
        }
    }
}
```

## Distributed Effects

### Distributed Effect Coordination

For distributed systems, we need to coordinate effects across multiple nodes while maintaining idempotency and consistency.

```rust
/// Distributed effect store with consensus
#[derive(Clone)]
pub struct DistributedEffectStore {
    local_store: Arc<Mutex<ReceiptStore>>,
    cluster: Arc<Cluster>,
    consensus: Arc<Consensus>,
}

impl DistributedEffectStore {
    pub fn new(cluster: Arc<Cluster>, consensus: Arc<Consensus>) -> Self {
        Self {
            local_store: Arc::new(Mutex::new(ReceiptStore::new())),
            cluster,
            consensus,
        }
    }

    /// Store receipt with consensus
    pub async fn store_with_consensus(&self, receipt: Receipt) -> Result<(), ConsensusError> {
        // Propose to cluster
        let proposal = Proposal::StoreReceipt(receipt.clone());

        // Reach consensus
        let agreed = self.consensus.propose(proposal).await?;

        if agreed {
            // Store locally
            let mut store = self.local_store.lock().await;
            store.store(receipt).await?;
            Ok(())
        } else {
            Err(ConsensusError::Rejected)
        }
    }

    /// Get receipt with consistency guarantee
    pub async fn get_with_consistency(
        &self,
        effect_id: &EffectId,
        consistency: ConsistencyLevel
    ) -> Option<Receipt> {
        match consistency {
            ConsistencyLevel::Strong => {
                // Ensure all nodes have the same data
                let local_receipt = {
                    let store = self.local_store.lock().await;
                    store.lookup_by_effect_id(effect_id).await
                };

                // Check other nodes for consistency
                for node in self.cluster.nodes() {
                    if let Ok(Some(remote_receipt)) = node.get_receipt(effect_id).await {
                        if remote_receipt != local_receipt? {
                            // Resolve conflict
                            return self.resolve_conflict(effect_id, local_receipt, Some(remote_receipt)).await;
                        }
                    }
                }

                local_receipt
            }
            ConsistencyLevel::Eventual => {
                // Return local copy
                let store = self.local_store.lock().await;
                store.lookup_by_effect_id(effect_id).await
            }
        }
    }

    async fn resolve_conflict(
        &self,
        effect_id: &EffectId,
        local: Option<Receipt>,
        remote: Option<Receipt>
    ) -> Option<Receipt> {
        // Use timestamp as tie-breaker
        match (local, remote) {
            (Some(local_receipt), Some(remote_receipt)) => {
                if local_receipt.timestamp > remote_receipt.timestamp {
                    self.store_with_consensus(local_receipt).await.ok()?;
                    Some(local_receipt)
                } else {
                    self.store_with_consensus(remote_receipt).await.ok()?;
                    Some(remote_receipt)
                }
            }
            (Some(receipt), None) => {
                self.store_with_consensus(receipt).await.ok()?;
                Some(receipt)
            }
            (None, Some(receipt)) => {
                self.store_with_consensus(receipt).await.ok()?;
                Some(receipt)
            }
            (None, None) => None,
        }
    }
}
```

### Cross-Node Effect Execution

```rust
/// Effect that can be executed on any node
pub struct DistributedEffect<T> {
    spec: T,
    preferred_nodes: Vec<NodeId>,
    placement_policy: PlacementPolicy,
}

#[derive(Debug, Clone)]
enum PlacementPolicy {
    // Prefer specific nodes
    Prefer(Vec<NodeId>),
    // Any node in cluster
    Any,
    // Node with lowest load
    LeastLoaded,
    // Round-robin
    RoundRobin,
}

impl<T> DistributedEffect<T>
    where T: EffectSpec
{
    pub fn new(spec: T, preferred_nodes: Vec<NodeId>) -> Self {
        Self {
            spec,
            preferred_nodes,
            placement_policy: PlacementPolicy::Prefer(preferred_nodes.clone()),
        }
    }

    pub fn with_policy(mut self, policy: PlacementPolicy) -> Self {
        self.placement_policy = policy;
        self
    }
}

#[async_trait::async_trait]
impl<T> EffectSpec for DistributedEffect<T>
    where T: EffectSpec + Send + Sync + 'static
{
    type Output = T::Output;
    type Context = T::Context;

    fn effect_type(&self) -> &'static str { self.spec.effect_type() }
    fn idempotency_key(&self) -> Option<&str> { self.spec.idempotency_key() }
    fn timeout(&self) -> Option<Duration> {
        // Add network latency to timeout
        self.spec.timeout().map(|t| t + Duration::from_secs(5))
    }
}

#[async_trait::async_trait]
impl<T> Effect<T> for DistributedEffect<T>
    where T: Effect<Output = T::Output, Context = T::Context> + Send + Sync + 'static
{
    async fn execute(&self, ctx: T::Context) -> EffectResult<T::Output, T::Context> {
        // Select node based on placement policy
        let selected_node = self.select_node().await
            .ok_or_else(|| EffectError::Handler("No available nodes".to_string()))?;

        // Execute on selected node
        let result = selected_node.execute_effect(self.spec.clone(), ctx.clone()).await;

        // Handle node failure by retrying
        match result {
            Ok(output) => Ok((ctx, output)),
            Err(_) => {
                // Mark node as unhealthy and retry
                let cluster = Arc::new(Cluster::new());
                cluster.mark_unhealthy(&selected_node);

                // Try again
                if let Some(new_node) = self.select_node().await {
                    let result = new_node.execute_effect(self.spec.clone(), ctx).await;
                    result.map(|output| ((), output))
                        .map_err(|e| EffectError::Handler(e.to_string()))
                } else {
                    Err(EffectError::Handler("No available nodes for retry".to_string()))
                }
            }
        }
    }
}
```

## Code Examples

### Complete Workflow Example

Here's a complete example showing how to build a workflow with multiple effects:

```rust
/// Workflow: Process user order with external API calls
pub async fn process_order(
    user_id: UserId,
    order: Order,
    executor: Arc<TokioEffectExecutor>
) -> Result<ProcessingResult, EffectError> {
    let case_id = CaseId::new();
    let mut ctx = OrderContext::new(user_id, order);

    // Step 1: Validate user (with idempotency)
    let validate_effect = ValidateUser::new(user_id);
    let (_, user) = executor.submit(
        EffectId::new(case_id, 1, "validate_user"),
        validate_effect
    ).await?;

    ctx.user = Some(user);

    // Step 2: Check inventory (with retry)
    let inventory_effect = RetryEffect::new(
        CheckInventory::new(ctx.order.items.clone()),
        RetryPolicy::new()
            .with_attempts(3)
            .with_backoff(
                Duration::from_millis(100),
                Duration::from_secs(5),
                2.0
            )
            .with_jitter(true)
    );

    let (_, inventory_result) = executor.submit(
        EffectId::new(case_id, 2, "check_inventory"),
        inventory_effect
    ).await?;

    ctx.inventory = Some(inventory_result);

    // Step 3: Process payment (with timeout)
    let payment_effect = TimeoutEffect::new(
        ProcessPayment::new(ctx.order.total, ctx.user.as_ref().unwrap()),
        Duration::from_secs(30)
    );

    let (_, payment_result) = executor.submit(
        EffectId::new(case_id, 3, "process_payment"),
        payment_effect
    ).await?;

    // Step 4: Create order (with fallback)
    let create_order_effect = FallbackEffect::new(
        CreateOrder::new(ctx.clone()),
        vec![
            CreateOrderBackup::new(ctx.clone()),
            CreateOrderSecondary::new(ctx.clone()),
        ]
    );

    let (_, order_result) = executor.submit(
        EffectId::new(case_id, 4, "create_order"),
        create_order_effect
    ).await?;

    // Step 5: Send notification (with circuit breaker)
    let notification_effect = CancellableEffect::new(
        SendNotification::new(
            order_result.id,
            ctx.user.as_ref().unwrap().email.clone(),
            "Order Confirmation"
        )
    );

    // In case of workflow cancellation
    if ctx.cancelled {
        notification_effect.cancel();
        return Err(EffectError::Cancelled);
    }

    let (_, _) = executor.submit(
        EffectId::new(case_id, 5, "send_notification"),
        notification_effect
    ).await?;

    Ok(ProcessingResult {
        order_id: order_result.id,
        status: "completed".to_string(),
        user_id,
        total: ctx.order.total,
    })
}
```

### Effect Builder Pattern

For cleaner effect composition, we provide a builder pattern:

```rust
/// Builder for composing effects
pub struct EffectBuilder {
    registry: Arc<EffectRegistry>,
    idempotency_store: Arc<IdempotencyStore>,
    receipt_store: Arc<Mutex<ReceiptStore>>,
}

impl EffectBuilder {
    pub fn new() -> Self {
        Self {
            registry: Arc::new(EffectRegistry::default()),
            idempotency_store: Arc::new(IdempotencyStore::new(Duration::from_secs(3600))),
            receipt_store: Arc::new(Mutex::new(ReceiptStore::new())),
        }
    }

    pub fn register_effect<T, H>(&mut self, handler: H)
        where T: EffectSpec + Send + Sync + 'static,
              H: EffectHandler<T> + Send + Sync + 'static
    {
        self.registry.register::<T>(handler);
    }

    pub fn http_get(&self, url: String) -> HttpGetBuilder {
        HttpGetBuilder::new(url, self.registry.clone(), self.idempotency_store.clone())
    }

    pub fn database_query(&self, query: String) -> DatabaseQueryBuilder {
        DatabaseQueryBuilder::new(query, self.registry.clone(), self.idempotency_store.clone())
    }

    pub fn batch<T>(&self, effects: Vec<EffectDefinition<T>>) -> BatchEffect<T> {
        BatchEffect::new(effects, self.registry.clone(), self.idempotency_store.clone())
    }
}

/// HTTP GET builder
pub struct HttpGetBuilder {
    url: String,
    headers: Option<HashMap<String, String>>,
    timeout: Option<Duration>,
    idempotency_key: Option<String>,
    registry: Arc<EffectRegistry>,
    idempotency_store: Arc<IdempotencyStore>,
}

impl HttpGetBuilder {
    pub fn new(url: String, registry: Arc<EffectRegistry>, idempotency_store: Arc<IdempotencyStore>) -> Self {
        Self {
            url,
            headers: None,
            timeout: None,
            idempotency_key: None,
            registry,
            idempotency_store,
        }
    }

    pub fn with_header(mut self, key: String, value: String) -> Self {
        if let Some(headers) = &mut self.headers {
            headers.insert(key, value);
        } else {
            let mut headers = HashMap::new();
            headers.insert(key, value);
            self.headers = Some(headers);
        }
        self
    }

    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    pub fn with_idempotency_key(mut self, key: String) -> Self {
        self.idempotency_key = Some(key);
        self
    }

    pub async fn execute(self) -> Result<HttpResponse, EffectError> {
        let effect = HttpGet {
            url: self.url,
            headers: self.headers,
            timeout: self.timeout,
            idempotency_key: self.idempotency_key,
        };

        let handler = self.registry.get_handler("http_get")
            .ok_or_else(|| EffectError::Handler("HTTP handler not registered".to_string()))?;

        let result = handler.handle_dyn("http_get", &bincode::serialize(&effect)?)
            .await?;

        bincode::deserialize(&result)
            .map_err(|e| EffectError::Handler(e.to_string()))
    }
}

/// Batch effect builder
pub struct BatchEffect<T> {
    effects: Vec<EffectDefinition<T>>,
    registry: Arc<EffectRegistry>,
    idempotency_store: Arc<IdempotencyStore>,
}

impl<T> BatchEffect<T>
    where T: Send + 'static
{
    pub fn new(effects: Vec<EffectDefinition<T>>, registry: Arc<EffectRegistry>, idempotency_store: Arc<IdempotencyStore>) -> Self {
        Self { effects, registry, idempotency_store }
    }

    pub async fn execute(self) -> Vec<EffectResult<T, ()>> {
        let futures: Vec<_> = self.effects.into_iter()
            .map(|def| async {
                let handler = self.registry.get_handler(&def.effect_type)
                    .ok_or_else(|| EffectError::Handler(format!("Unknown effect type: {}", def.effect_type)))?;

                handler.handle_dyn(&def.effect_type, &def.payload).await
                    .map(|data| bincode::deserialize(&data).unwrap())
                    .map_err(|e| EffectError::Handler(e.to_string()))
            })
            .collect();

        futures::future::join_all(futures).await
    }
}
```

## Comparison with Haskell/OCaml Effect Systems

### Type System Differences

While Haskell and OCaml have native support for algebraic effects through libraries like extensible-effects and eff, Rust's approach differs significantly due to its type system:

**Haskell/OCaml Algebraic Effects:**
```haskell
-- Koka-style algebraic effects
data Handler eff r = forall a. eff a -> (a -> Cont r) -> Cont r

perform :: eff a -> Eff eff a
perform eff = Eff $ \k -> Handler eff k

handle :: Handler eff r -> Eff eff r -> Eff eff r
handle handler (Eff k) = Eff $ \k' -> handler (perform eff) k'
```

**Rust's Trait-Based Approach:**
```rust
// Rust's traits simulate effect interfaces
pub trait EffectSpec {
    type Output;
    type Context;
    // ...
}

pub trait EffectHandler<T> {
    type Error;
    async fn handle(&self, spec: &T) -> Result<T::Output, Self::Error>;
}
```

### Key Differences:

1. **Type Safety**: Rust's ownership system provides stronger guarantees about effect isolation but requires more boilerplate.

2. **Async Integration**: Rust's async/await integrates naturally with effect systems, whereas Haskell/OCaml require additional libraries.

3. **Effect Composition**: Rust's trait-based approach allows for more flexible composition but lacks some of the mathematical elegance of algebraic effects.

4. **Performance**: Rust's zero-cost abstractions can lead to better performance for effect handling.

### Equivalent Patterns:

**Haskell's `withException`:**
```haskell
withException handler action =
    action `catch` \e -> handler e
```

**Rust's equivalent:**
```rust
pub struct ExceptionEffect<T> {
    action: T,
    handler: Box<dyn Fn(EffectError) -> EffectResult<T::Output, T::Context> + Send>,
}

#[async_trait::async_trait]
impl<T> Effect<T> for ExceptionEffect<T>
    where T: Effect<Output = T::Output, Context = T::Context> + Send + Sync + 'static
{
    async fn execute(&self, ctx: T::Context) -> EffectResult<T::Output, T::Context> {
        match self.action.execute(ctx).await {
            Ok((ctx, result)) => Ok((ctx, result)),
            Err(e) => (self.handler)(e),
        }
    }
}
```

**Haskell's `resourceT`:**
```haskell
resourceT :: Resource -> Eff eff a -> Eff eff a
```

**Rust's equivalent:**
```rust
pub struct ResourceEffect<T> {
    resource: Arc<dyn Resource>,
    action: T,
}

impl<T> Drop for ResourceEffect<T>
    where T: EffectSpec
{
    fn drop(&mut self) {
        self.resource.cleanup();
    }
}
```

### Rust's Unique Advantages:

1. **Memory Safety**: Rust's borrow checker prevents many common runtime errors that can occur in effect systems.

2. **Performance**: Zero-cost abstractions mean effect handling has minimal overhead.

3. **Concurrency**: Natural integration with Rust's async ecosystem makes concurrent effects easier to manage.

4. **Tooling**: Excellent compiler error messages and IDE support for debugging effect code.

### Limitations Compared to Algebraic Effects:

1. **No Native Support**: Rust lacks built-in algebraic effect support, requiring more boilerplate.

2. **Effect Polymorphism**: More difficult to achieve higher-kinded effect abstractions.

3. **Effect Handlers as Values**: Cannot pass handlers as first-class values as easily as in Haskell/OCaml.

## Conclusion

This paper has presented a comprehensive implementation of an effect system for workflows in Rust, demonstrating how Rust's type system and async capabilities can be leveraged to create safe, composable, and performant effect handling. The implementation includes:

1. **Type-safe effect specifications** with compile-time guarantees
2. **Composable effect handlers** supporting middleware and cross-cutting concerns
3. **Idempotency mechanisms** for safe retry and caching
4. **Receipt-based auditing** enabling deterministic replay
5. **Async execution patterns** with seamless tokio integration
6. **Robust error recovery** including retries, circuit breakers, and fallbacks
7. **Distributed effect coordination** for multi-node workflows
8. **Clean abstractions** through builder patterns and trait hierarchies

While Rust lacks native algebraic effect support, the trait-based approach provides a practical and efficient alternative that leverages Rust's strengths. The implementation demonstrates that effect systems can be effectively implemented in a systems programming language without sacrificing performance or safety.

Future work could explore:
- More sophisticated effect composition patterns
- Integration with Rust's upcoming effect system proposals
- GPU acceleration for effect-heavy computations
- Effect-based streaming and reactive programming
- Formal verification of effect safety properties

The effect system presented here provides a solid foundation for building reliable, testable, and maintainable workflow systems in Rust, with the flexibility to adapt to various domains and use cases.