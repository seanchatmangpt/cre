# Build stage
FROM node:18-alpine as builder

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache \
    python3 \
    make \
    g++ \
    cairo-dev \
    jpeg-dev \
    pango-dev \
    giflib-dev

# Copy package files
COPY admin-console/package*.json ./

# Install dependencies
RUN npm ci --prefer-offline --no-audit

# Copy source code
COPY admin-console ./

# Build application
RUN npm run build

# Runtime stage
FROM node:18-alpine

WORKDIR /app

# Create non-root user
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser

# Install runtime dependencies
RUN apk add --no-cache \
    dumb-init \
    curl \
    ca-certificates && \
    apk add --no-cache --from=builder \
    /lib/ld-musl-x86_64.so.1

# Copy built application from builder
COPY --from=builder --chown=appuser:appuser /app/dist ./dist
COPY --from=builder --chown=appuser:appuser /app/node_modules ./node_modules
COPY --from=builder --chown=appuser:appuser /app/package*.json ./

# Copy health check scripts
COPY --chown=appuser:appuser scripts/health-check.sh ./scripts/
COPY --chown=appuser:appuser scripts/startup.sh ./scripts/

RUN chmod +x ./scripts/health-check.sh ./scripts/startup.sh

# Create necessary directories
RUN mkdir -p /tmp /app/.cache && \
    chown -R appuser:appuser /tmp /app/.cache

# Security: Set umask for created files
RUN umask 0077

# Switch to non-root user
USER appuser

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:3000/health || exit 1

# Expose ports
EXPOSE 3000 8080

# Use dumb-init to handle signals properly
ENTRYPOINT ["/usr/bin/dumb-init", "--"]

# Start application
CMD ["node", "-r", "dotenv/config", "dist/index.js"]

# Metadata
LABEL org.opencontainers.image.title="Admin Console"
LABEL org.opencontainers.image.description="Admin console application for cluster management"
LABEL org.opencontainers.image.version="1.0.0"
LABEL org.opencontainers.image.vendor="Your Organization"
LABEL org.opencontainers.image.source="https://github.com/your-org/admin-console"
