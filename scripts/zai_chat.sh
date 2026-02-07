#!/usr/bin/env bash
# Z.AI Chat API - shell (curl) client
# Usage: ZAI_API_KEY=your_key ./scripts/zai_chat.sh [simple|stream|multi|prompt "your text"]
# Examples:
#   ZAI_API_KEY=key ./scripts/zai_chat.sh simple
#   ZAI_API_KEY=key ./scripts/zai_chat.sh prompt "Explain recursion"
set -e

API_KEY="${ZAI_API_KEY:-}"
if [ -z "$API_KEY" ]; then
  echo "ZAI_API_KEY not set. Set it and run again."
  exit 1
fi

BASE_URL="https://api.z.ai/api/paas/v4/chat/completions"
MODEL="${ZAI_MODEL:-glm-4.7-flash}"
MODE="${1:-simple}"
PROMPT="${2:-}"

case "$MODE" in
  simple)
    curl -sS -X POST "$BASE_URL" \
      -H "Authorization: Bearer $API_KEY" \
      -H "Accept-Language: en-US,en" \
      -H "Content-Type: application/json" \
      -d "{
        \"model\": \"$MODEL\",
        \"messages\": [{\"role\": \"user\", \"content\": \"Hello\"}],
        \"temperature\": 1.0,
        \"max_tokens\": 1024
      }"
    ;;
  stream)
    curl -sS -X POST "$BASE_URL" \
      -H "Authorization: Bearer $API_KEY" \
      -H "Accept-Language: en-US,en" \
      -H "Content-Type: application/json" \
      -d "{
        \"model\": \"$MODEL\",
        \"messages\": [{\"role\": \"user\", \"content\": \"Write a poem about spring\"}],
        \"stream\": true
      }"
    ;;
  multi)
    curl -sS -X POST "$BASE_URL" \
      -H "Authorization: Bearer $API_KEY" \
      -H "Accept-Language: en-US,en" \
      -H "Content-Type: application/json" \
      -d "{
        \"model\": \"$MODEL\",
        \"messages\": [
          {\"role\": \"system\", \"content\": \"You are a professional programming assistant\"},
          {\"role\": \"user\", \"content\": \"What is recursion?\"},
          {\"role\": \"assistant\", \"content\": \"Recursion is a programming technique where a function calls itself to solve problems...\"},
          {\"role\": \"user\", \"content\": \"Can you give me an example of Python recursion?\"}
        ]
      }"
    ;;
  prompt)
    CONTENT="${PROMPT:-Hello}"
    if command -v jq >/dev/null 2>&1; then
      BODY=$(jq -n --arg c "$CONTENT" --arg m "$MODEL" '{model:$m,messages:[{role:"user",content:$c}],temperature:1.0,max_tokens:1024}')
    else
      ESCAPED=$(echo "$CONTENT" | sed 's/\\/\\\\/g; s/"/\\"/g; s/\n/\\n/g')
      BODY="{\"model\":\"$MODEL\",\"messages\":[{\"role\":\"user\",\"content\":\"$ESCAPED\"}],\"temperature\":1.0,\"max_tokens\":1024}"
    fi
    curl -sS -X POST "$BASE_URL" \
      -H "Authorization: Bearer $API_KEY" \
      -H "Accept-Language: en-US,en" \
      -H "Content-Type: application/json" \
      -d "$BODY"
    ;;
  *)
    echo "Usage: ZAI_API_KEY=key $0 [simple|stream|multi|prompt \"your text\"]"
    exit 1
    ;;
esac
