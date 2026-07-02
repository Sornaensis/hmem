#!/bin/sh
set -eu

APP_NAME="hmem-healthcheck"

log() {
  printf '%s: %s\n' "$APP_NAME" "$*" >&2
}

health_url() {
  if [ -n "${HMEM_HEALTHCHECK_URL:-}" ]; then
    printf '%s' "$HMEM_HEALTHCHECK_URL"
    return 0
  fi

  hc_host=${HMEM_SERVER_HOST:-127.0.0.1}
  hc_port=${HMEM_SERVER_PORT:-8420}
  case "$hc_host" in
    0.0.0.0) hc_host=127.0.0.1 ;;
    ::) hc_host='[::1]' ;;
    *:*)
      case "$hc_host" in
        \[*\]) : ;;
        *) hc_host="[$hc_host]" ;;
      esac
      ;;
  esac
  printf 'http://%s:%s/api/v1/health' "$hc_host" "$hc_port"
}

fetch_health() {
  fh_url=$1
  fh_timeout=${HMEM_HEALTHCHECK_TIMEOUT_SECONDS:-5}
  if command -v curl >/dev/null 2>&1; then
    curl -fsS --max-time "$fh_timeout" "$fh_url"
    return $?
  fi
  if command -v wget >/dev/null 2>&1; then
    wget -q -T "$fh_timeout" -O - "$fh_url"
    return $?
  fi
  log "curl or wget is required"
  return 1
}

status_is_ok() {
  si_body=$1

  if command -v jq >/dev/null 2>&1; then
    printf '%s' "$si_body" | jq -e '.status == "ok"' >/dev/null 2>&1
    return $?
  fi

  if command -v python3 >/dev/null 2>&1; then
    printf '%s' "$si_body" | python3 -c 'import json,sys; sys.exit(0 if json.load(sys.stdin).get("status") == "ok" else 1)'
    return $?
  fi

  if command -v python >/dev/null 2>&1; then
    printf '%s' "$si_body" | python -c 'import json,sys; sys.exit(0 if json.load(sys.stdin).get("status") == "ok" else 1)'
    return $?
  fi

  log "jq, python3, or python is required to parse health JSON"
  return 1
}

main() {
  url=$(health_url)
  body=$(fetch_health "$url") || {
    log "request failed: $url"
    exit 1
  }

  if status_is_ok "$body"; then
    exit 0
  fi

  log "health endpoint did not report status=ok"
  exit 1
}

main "$@"
