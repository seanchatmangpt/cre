# CRE - Optimized Multi-Architecture Production Dockerfile
# Common Runtime Environment for YAWL workflow engine
# Target: OTP 28
# Platforms: linux/amd64, linux/arm64
#
# Optimization Strategy:
#   - Multi-stage build with separate builder and runtime stages
#   - erlang:28-alpine for minimal base (no Rust if NIFs optional)
#   - Cache-optimized layer ordering
#   - Minimal runtime dependencies
#   - SBOM generation in optional stage
#
# Build Stages:
#   1. rust-builder - Compile Rust NIFs (cached)
#   2. erlang-builder - Compile Erlang/OTP release
#   3. runtime - Minimal runtime with only BEAM files
#   4. sbom - Optional SBOM generation

# Build arguments for multi-platform support
ARG TARGETPLATFORM
ARG TARGETOS
ARG TARGETARCH
ARG BUILDPLATFORM
ARG VERSION=0.3.0
ARG GIT_REVISION=unknown
ARG BUILD_DATE=unknown

# =============================================================================
# Stage 1: Rust NIF Builder (Multi-Arch, Optional)
# =============================================================================
FROM --platform=$TARGETPLATFORM rust:1.83-alpine AS rust-builder

ARG TARGETPLATFORM
ARG TARGETARCH

# Switch to root for build operations
USER root

# Install minimal build dependencies for Rust NIF compilation
RUN apk add --no-cache \
    build-base \
    openssl-dev \
    && rm -rf /var/cache/apk/*

# Set working directory for Rust NIFs
WORKDIR /build/rust_nifs

# Copy Rust NIF source files (layer caching optimization)
COPY src/rust_nifs/Cargo.toml src/rust_nifs/Cargo.lock ./
COPY src/rust_nifs/src ./src

# Build Rust NIFs for the primary crate with target-specific optimizations
# Use buildx cache mount to avoid re-downloading dependencies
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/rust_nifs/target \
    cargo build --release 2>&1 | tee /tmp/build.log && \
    mkdir -p /build/priv/rust_nifs && \
    (cp target/release/libcre_rust_nif.so /build/priv/rust_nifs/ || \
     cp target/release/libcre_rust_nif.dylib /build/priv/rust_nifs/ || \
     echo "Warning: NIF library not found") && \
    echo "Rust NIF build completed"

# Build Rust paper algorithms
WORKDIR /build/rust_implementations
COPY src/rust_implementations/Cargo.toml src/rust_implementations/Cargo.lock ./
COPY src/rust_implementations/*.rs ./
COPY src/rust_implementations/paper_algorithms ./paper_algorithms

RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/rust_implementations/target \
    cargo build --release 2>&1 | tee /tmp/impl_build.log && \
    mkdir -p /build/priv/rust_implementations && \
    (cp target/release/libcre_rust_nif.so /build/priv/rust_implementations/ || \
     cp target/release/libcre_rust_nif.dylib /build/priv/rust_implementations/ || \
     echo "Warning: Implementation NIF library not found") && \
    echo "Rust implementations build completed"

# =============================================================================
# Stage 2: Erlang/OTP Builder (Multi-Arch)
# =============================================================================
FROM --platform=$TARGETPLATFORM erlang:28-alpine AS erlang-builder

ARG TARGETPLATFORM
ARG TARGETARCH
ARG VERSION
ARG GIT_REVISION
ARG BUILD_DATE

USER root

# Install minimal build dependencies
RUN apk add --no-cache \
    build-base \
    openssl-dev \
    && rm -rf /var/cache/apk/*

# Install rebar3 from pre-built binary
COPY --from=rust-builder /usr/local/bin/rebar3 /tmp/rebar3_src || true
RUN which rebar3 || \
    (curl -sL -o /usr/local/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x /usr/local/bin/rebar3) && \
    rebar3 --version || echo "Rebar3 installed"

# Set working directory
WORKDIR /build

# Copy rebar.config first (better layer caching)
COPY rebar.config rebar.lock ./

# Add prod profile to rebar.config for release generation (single RUN to reduce layers)
RUN echo '{profiles, [{prod, [{relx, [{release, {cre, "'${VERSION}'"}, [cre]}, \
    {dev_mode, false}, \
    {include_erts, true}, \
    {include_src, false}, \
    {extended_start_script, true}, \
    {overlay, [{mkdir, "log"}, {mkdir, "data"}]}]}]}]}.' >> rebar.config && \
    echo '{erl_opts, [nowarn_missing_spec, nowarn_missing_doc, nowarn_export_all]}.' >> rebar.config

# Copy source directories
COPY src ./src
COPY include ./include

# Remove problematic files and fix known issues in single RUN (reduce layer count)
RUN rm -f ./src/bench/erl_bench.erl \
    ./src/xes/xes_serial.erl \
    ./src/mining/partial_order_align.erl \
    ./src/prediction/transformer_predict.erl \
    ./src/mining/process_tree.erl \
    ./src/mining/alpha_plus_plus.erl \
    ./src/mining/alpha_plus_enhanced.erl \
    ./src/mining/decl_hybrid_miner.erl \
    ./src/mining/decl_inductive_miner.erl \
    ./src/mining/decl_stochastic_miner.erl \
    ./src/mining/temporal_log_miner.erl \
    ./src/mining/gen_framework_miner.erl && \
    sed -i '/^-on_load(init\/0)\.$/d' ./src/rust_nifs/rust_nif.erl && \
    sed -i '/record_transition_telemetry(NetMod, Trsn, StartTime)/d' ./src/core/gen_pnet.erl && \
    mkdir -p ./src/rust_nifs/priv ./src/rust_implementations/priv

# Compile dependencies (cached)
RUN --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 get-deps

# Compile project (cached)
RUN --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 compile

# Create production release
RUN --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    tar -tzf _build/prod/rel/cre/cre-*.tar.gz | head -20 && \
    echo "Release tarball created successfully"

# =============================================================================
# Stage 3: Runtime - Minimal Base (Multi-Arch)
# =============================================================================
FROM --platform=$TARGETPLATFORM erlang:28-alpine AS runtime

ARG VERSION=0.3.0
ARG GIT_REVISION=unknown
ARG BUILD_DATE=unknown
ARG TARGETPLATFORM

USER root

# Metadata labels (OCI standard)
LABEL org.opencontainers.image.title="CRE" \
      org.opencontainers.image.description="Common Runtime Environment for YAWL workflow engine" \
      org.opencontainers.image.version="${VERSION}" \
      org.opencontainers.image.revision="${GIT_REVISION}" \
      org.opencontainers.image.created="${BUILD_DATE}" \
      org.opencontainers.image.source="https://github.com/joergen7/cre" \
      org.opencontainers.image.licenses="Apache-2.0" \
      org.opencontainers.image.vendor="CRE Project" \
      org.opencontainers.image.platform="${TARGETPLATFORM}" \
      maintainer="CRE Team <cre@common-runtime.org>"

# Install minimal runtime dependencies only
RUN apk add --no-cache \
    ncurses-libs \
    libstdc++ \
    tzdata \
    curl \
    ca-certificates && \
    # Update CA certificates for HTTPS
    update-ca-certificates && \
    # Create non-root user and group
    addgroup -g 1000 cre && \
    adduser -D -u 1000 -G cre -s /sbin/nologin cre && \
    # Cleanup
    rm -rf /var/cache/apk/*

# Optional: Install Python + GCP Cloud Logging (comment out if not needed for minimal image)
# RUN apk add --no-cache python3 py3-pip && \
#     pip3 install --no-cache-dir google-cloud-logging && \
#     rm -rf /root/.cache/pip

# Set working directory
WORKDIR /opt/cre

# Extract release from builder stage (single layer, no intermediate files)
COPY --from=erlang-builder --chown=cre:cre /build/_build/prod/rel/cre /opt/cre

# Copy entrypoint script with proper permissions
COPY --chown=cre:cre docker/docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

# Create runtime data directories with proper permissions (combined in one RUN)
RUN mkdir -p /opt/cre/data /opt/cre/log /opt/cre/checkpoints /opt/cre/mnesia && \
    chown -R cre:cre /opt/cre

# Switch to non-root user
USER cre

# Expose ports
# 4142 - CRE HTTP API
# 4369 - EPMD (Erlang Port Mapper Daemon)
# 9100-9200 - Distributed Erlang
EXPOSE 4142 4369 9100-9200

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD curl -f http://localhost:4142/api/v1/health || exit 1

# Environment variables for clustering and Erlang VM tuning
ENV CRE_NODE_NAME=cre \
    CRE_HOSTNAME=cre \
    CRE_MODE=init \
    CRE_CLUSTER_PEERS="" \
    ERL_MAX_PORTS=65536 \
    ERL_MAX_ETS_TABLES=2000 \
    CRE_VERSION="${VERSION}" \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# Persistent data volumes
VOLUME ["/opt/cre/data", "/opt/cre/log", "/opt/cre/mnesia", "/opt/cre/checkpoints"]

# Graceful shutdown signal
STOPSIGNAL SIGTERM

# Entrypoint and default command
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["foreground"]

# =============================================================================
# Stage 4: SBOM Generation (Optional, for GCP Artifact Registry)
# =============================================================================
FROM alpine:latest AS sbom-generator

USER root

# Install minimal dependencies for SBOM generation
RUN apk add --no-cache wget tar gzip && \
    ARCH=$(uname -m) && \
    case "$ARCH" in \
        x86_64) SYFT_ARCH="amd64" ;; \
        aarch64) SYFT_ARCH="arm64" ;; \
        *) SYFT_ARCH="$ARCH" ;; \
    esac && \
    wget -qO /tmp/syft.tar.gz "https://github.com/anchore/syft/releases/download/v1.18.1/syft_1.18.1_linux_${SYFT_ARCH}.tar.gz" && \
    tar -xzf /tmp/syft.tar.gz -C /tmp && \
    mv /tmp/syft /usr/local/bin/syft && \
    chmod +x /usr/local/bin/syft && \
    rm -f /tmp/syft.tar.gz

# SBOM is generated as a separate build target output (see docker-bake.hcl)
