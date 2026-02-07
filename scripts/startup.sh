#!/bin/bash

set -euo pipefail

# Color codes
BLUE='\033[0;34m'
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Environment setup
log_info "Setting up application environment..."

# Create necessary directories
mkdir -p /tmp /app/.cache

# Set NODE_ENV if not already set
export NODE_ENV="${NODE_ENV:-production}"
log_info "NODE_ENV: $NODE_ENV"

# Set PORT if not already set
export PORT="${PORT:-3000}"
log_info "PORT: $PORT"

# Load environment variables from .env if it exists
if [ -f "/app/.env" ]; then
    log_info "Loading .env file..."
    set -a
    source /app/.env
    set +a
fi

# Verify required environment variables
REQUIRED_VARS=(
    "NODE_ENV"
    "PORT"
)

for var in "${REQUIRED_VARS[@]}"; do
    if [ -z "${!var:-}" ]; then
        log_error "Required environment variable not set: $var"
        exit 1
    fi
done

# Verify application health
log_info "Checking application availability..."

if [ ! -d "/app/dist" ]; then
    log_error "Application build directory not found: /app/dist"
    exit 1
fi

if [ ! -f "/app/package.json" ]; then
    log_error "package.json not found"
    exit 1
fi

# Check Node modules
if [ ! -d "/app/node_modules" ]; then
    log_error "node_modules directory not found, installing dependencies..."
    cd /app
    npm ci --production
fi

# Run database migrations if needed
if [ -f "/app/scripts/migrate.sh" ]; then
    log_info "Running database migrations..."
    bash /app/scripts/migrate.sh || log_error "Migration failed"
fi

# Run seed scripts if needed
if [ -f "/app/scripts/seed.sh" ] && [ "${RUN_SEED:-false}" = "true" ]; then
    log_info "Running seed scripts..."
    bash /app/scripts/seed.sh || log_error "Seeding failed"
fi

# Set proper permissions
log_info "Setting file permissions..."
chmod -R 755 /app/dist 2>/dev/null || true
chmod -R 755 /app/scripts 2>/dev/null || true

# Start application
log_success "Application startup preparation complete"
log_info "Starting application on port $PORT..."

# Handle SIGTERM and SIGINT gracefully
trap 'log_info "Received signal, shutting down gracefully..."; kill $! 2>/dev/null' SIGTERM SIGINT

# Start the application
exec node -r dotenv/config /app/dist/index.js
