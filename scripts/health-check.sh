#!/bin/bash

set -uo pipefail

# Configuration
HEALTH_CHECK_ENDPOINT="${HEALTH_CHECK_ENDPOINT:-http://localhost:3000/health}"
READINESS_ENDPOINT="${READINESS_ENDPOINT:-http://localhost:3000/ready}"
STARTUP_ENDPOINT="${STARTUP_ENDPOINT:-http://localhost:3000/startup}"
TIMEOUT="${TIMEOUT:-5}"
RETRIES="${RETRIES:-3}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Check health
check_health() {
    local endpoint="$1"
    local name="$2"

    for attempt in $(seq 1 $RETRIES); do
        if curl -sf --connect-timeout "$TIMEOUT" "$endpoint" &> /dev/null; then
            echo -e "${GREEN}✓${NC} $name check passed"
            return 0
        fi

        if [ "$attempt" -lt "$RETRIES" ]; then
            sleep 1
        fi
    done

    echo -e "${RED}✗${NC} $name check failed"
    return 1
}

# Main
case "${1:-health}" in
    health)
        check_health "$HEALTH_CHECK_ENDPOINT" "Health"
        exit $?
        ;;
    readiness)
        check_health "$READINESS_ENDPOINT" "Readiness"
        exit $?
        ;;
    startup)
        check_health "$STARTUP_ENDPOINT" "Startup"
        exit $?
        ;;
    *)
        echo "Usage: $0 {health|readiness|startup}"
        exit 1
        ;;
esac
