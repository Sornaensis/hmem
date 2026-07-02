#!/bin/sh
set -eu
set -f

APP_NAME="hmem-entrypoint"
DEFAULT_HMEM_HOME="/var/lib/hmem"
CONFIG_BASENAME="config.yaml"

log() {
  printf '%s: %s\n' "$APP_NAME" "$*" >&2
}

warn() {
  log "WARNING: $*"
}

fail() {
  log "ERROR: $*"
  exit 1
}

get_env() {
  eval "printf '%s' \"\${$1-}\""
}

env_is_set_nonempty() {
  eval "[ \"\${$1+x}\" = x ] && [ -n \"\${$1}\" ]"
}

default_env() {
  de_var=$1
  de_default=$2
  eval "de_current=\${$de_var-}"
  if [ -z "$de_current" ]; then
    export "$de_var=$de_default"
  fi
}

lower() {
  printf '%s' "$1" | tr '[:upper:]' '[:lower:]'
}

trim() {
  printf '%s' "$1" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//'
}

read_secret_file() {
  rs_var=$1
  rs_file_var=$2
  rs_file=$(get_env "$rs_file_var")
  [ -n "$rs_file" ] || return 0
  [ -f "$rs_file" ] || fail "$rs_file_var points to a missing file"
  [ -r "$rs_file" ] || fail "$rs_file_var points to an unreadable file"
  rs_value=$(cat "$rs_file") || fail "could not read $rs_file_var"
  [ -n "$rs_value" ] || fail "$rs_file_var points to an empty file"
  export "$rs_var=$rs_value"
}

load_config_secret_files() {
  read_secret_file HMEM_DB_PASSWORD HMEM_DB_PASSWORD_FILE
  read_secret_file HMEM_API_KEY HMEM_API_KEY_FILE
  read_secret_file HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET_FILE
  read_secret_file HMEM_AUTH_DEPLOYED_CLIENT_SECRET HMEM_AUTH_DEPLOYED_CLIENT_SECRET_FILE
}

load_mcp_secret_files() {
  read_secret_file HMEM_MCP_AUTH_TOKEN HMEM_MCP_AUTH_TOKEN_FILE
  read_secret_file HMEM_AUTH_TOKEN HMEM_AUTH_TOKEN_FILE
}

load_secret_files() {
  load_config_secret_files
  load_mcp_secret_files
}

apply_defaults() {
  default_env HMEM_HOME "$DEFAULT_HMEM_HOME"
  default_env HOME "$HMEM_HOME"

  default_env HMEM_SERVER_HOST "0.0.0.0"
  default_env HMEM_SERVER_PORT "8420"
  default_env HMEM_DB_HOST "postgres"
  default_env HMEM_DB_PORT "5432"
  default_env HMEM_DB_NAME "hmem"
  default_env HMEM_DB_USER "hmem"
  default_env HMEM_DB_SSLMODE "disable"
  default_env HMEM_POOL_SIZE "10"
  default_env HMEM_POOL_IDLE_TIMEOUT "60"
  default_env HMEM_POOL_STATEMENT_TIMEOUT_MS "30000"
  default_env HMEM_LOG_LEVEL "info"
  default_env HMEM_LOG_MAX_SIZE_MB "10"
  default_env HMEM_LOG_BACKUP_COUNT "5"
  default_env HMEM_CORS_ALLOWED_ORIGINS ""
  default_env HMEM_WEB_ENABLED "true"
  default_env HMEM_WEB_STATIC_DIR "/opt/hmem/static"
  default_env HMEM_RATE_LIMIT_ENABLED "false"
  default_env HMEM_RATE_LIMIT_REQUESTS_PER_SECOND "10.0"
  default_env HMEM_RATE_LIMIT_BURST "20"

  default_env HMEM_AUTH_MODE "local"
  default_env HMEM_AUTH_ENABLED "true"
  default_env HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED "false"
  default_env HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP "false"
  default_env HMEM_AUTH_DEPLOYED_TOKEN_LOOKUP "database"
  default_env HMEM_AUTH_DEPLOYED_SCOPES "openid,profile,email"
  default_env HMEM_AUTH_DEPLOYED_SESSION_COOKIE_NAME "hmem_session"
  default_env HMEM_AUTH_DEPLOYED_CSRF_COOKIE_NAME "hmem_csrf"
  default_env HMEM_AUTH_DEPLOYED_CSRF_HEADER_NAME "X-CSRF-Token"
  default_env HMEM_AUTH_DEPLOYED_SESSION_TTL_SECONDS "28800"
  default_env HMEM_AUTH_DEPLOYED_COOKIE_SECURE "true"
  default_env HMEM_AUTH_DEPLOYED_COOKIE_SAME_SITE "Lax"

  default_env HMEM_FRONTEND_LOGOUT_REDIRECT_URL "/"
  default_env HMEM_FRONTEND_AUTH_TOKEN_STORAGE "local"
  default_env HMEM_FRONTEND_AUTH_TOKEN_STORAGE_KEY "hmem-auth-token"
  default_env HMEM_FRONTEND_AUTH_TOKEN_URL_PARAMS "hmem_token,auth_token,access_token"
  default_env HMEM_FRONTEND_REQUIRE_AUTH_STATE "true"

  default_env HMEM_MIGRATIONS_DIR "/opt/hmem/migrations"
  default_env HMEM_DB_CONNECT_RETRIES "30"
  default_env HMEM_DB_CONNECT_RETRY_DELAY_SECONDS "2"
  default_env HMEM_SERVER_URL "http://127.0.0.1:${HMEM_SERVER_PORT}"
}

bool_env() {
  be_var=$1
  be_value=$(get_env "$be_var")
  case "$(lower "$be_value")" in
    true|1|yes) printf 'true' ;;
    false|0|no) printf 'false' ;;
    *) fail "$be_var must be a boolean: true, false, 1, 0, yes, or no" ;;
  esac
}

int_env() {
  ie_var=$1
  ie_min=$2
  ie_max=$3
  ie_value=$(get_env "$ie_var")
  case "$ie_value" in
    ''|*[!0-9]*) fail "$ie_var must be an integer" ;;
  esac
  [ "$ie_value" -ge "$ie_min" ] || fail "$ie_var must be >= $ie_min"
  [ "$ie_value" -le "$ie_max" ] || fail "$ie_var must be <= $ie_max"
  printf '%s' "$ie_value"
}

decimal_env() {
  de_var=$1
  de_min=$2
  de_max=$3
  de_value=$(get_env "$de_var")
  if ! awk -v v="$de_value" -v lo="$de_min" -v hi="$de_max" '
    BEGIN {
      if (v !~ /^[0-9]+([.][0-9]+)?$/) exit 2
      if ((v + 0) < lo || (v + 0) > hi) exit 3
    }
  '; then
    fail "$de_var must be a number in range $de_min..$de_max"
  fi
  printf '%s' "$de_value"
}

one_of() {
  oo_var=$1
  oo_value=$(get_env "$oo_var")
  shift
  for oo_allowed in "$@"; do
    [ "$oo_value" = "$oo_allowed" ] && return 0
  done
  fail "$oo_var must be one of: $*"
}

validate_nonempty() {
  vn_var=$1
  [ -n "$(get_env "$vn_var")" ] || fail "$vn_var must be non-empty"
}

is_ipv4_loopback() {
  i4_host=$1
  i4_oldifs=$IFS
  IFS=.
  set -- $i4_host
  IFS=$i4_oldifs
  [ "$#" -eq 4 ] || return 1
  for i4_octet in "$@"; do
    case "$i4_octet" in
      ''|*[!0-9]*) return 1 ;;
    esac
    [ "$i4_octet" -ge 0 ] && [ "$i4_octet" -le 255 ] || return 1
  done
  [ "$1" -eq 127 ]
}

is_loopback_host() {
  il_host=$(lower "$(trim "$1")")
  case "$il_host" in
    localhost|::1|\[::1\]) return 0 ;;
  esac
  is_ipv4_loopback "$il_host"
}

origin_host() {
  oh_origin=$(trim "$1")
  oh_without_scheme=$oh_origin
  case "$oh_without_scheme" in
    *://*) oh_without_scheme=${oh_without_scheme#*://} ;;
  esac
  oh_authority=${oh_without_scheme%%/*}
  case "$oh_authority" in
    \[*\]*)
      oh_tmp=${oh_authority#\[}
      printf '%s' "${oh_tmp%%\]*}"
      ;;
    *)
      printf '%s' "${oh_authority%%:*}"
      ;;
  esac
}

cors_has_remote_or_wildcard() {
  cr_csv=$1
  [ -n "$cr_csv" ] || return 1
  cr_oldifs=$IFS
  IFS=,
  set -- $cr_csv
  IFS=$cr_oldifs
  for cr_origin in "$@"; do
    cr_origin=$(trim "$cr_origin")
    [ -n "$cr_origin" ] || continue
    [ "$cr_origin" = "*" ] && return 0
    cr_host=$(origin_host "$cr_origin")
    [ -n "$cr_host" ] || return 0
    is_loopback_host "$cr_host" || return 0
  done
  return 1
}

cors_has_wildcard() {
  cw_csv=$1
  [ -n "$cw_csv" ] || return 1
  cw_oldifs=$IFS
  IFS=,
  set -- $cw_csv
  IFS=$cw_oldifs
  for cw_origin in "$@"; do
    [ "$(trim "$cw_origin")" = "*" ] && return 0
  done
  return 1
}

csv_contains_token() {
  ct_csv=$1
  ct_token=$2
  [ -n "$ct_csv" ] || return 1
  ct_oldifs=$IFS
  IFS=,
  set -- $ct_csv
  IFS=$ct_oldifs
  for ct_item in "$@"; do
    [ "$(trim "$ct_item")" = "$ct_token" ] && return 0
  done
  return 1
}

validate_https_url() {
  vh_var=$1
  vh_value=$(get_env "$vh_var")
  [ -n "$vh_value" ] || return 0
  case "$vh_value" in
    https://*) return 0 ;;
    *) fail "$vh_var must start with https://" ;;
  esac
}

validate_scalar_no_newline() {
  vs_var=$1
  vs_value=$(get_env "$vs_var")
  case "$vs_value" in
    *"
"*) fail "$vs_var must not contain newlines" ;;
  esac
}

validate_csv_scalars_no_newline() {
  vc_var=$1
  vc_value=$(get_env "$vc_var")
  case "$vc_value" in
    *"
"*) fail "$vc_var must not contain newlines" ;;
  esac
}

validate_common_config() {
  vc_mode=${1:-server}

  HMEM_SERVER_PORT=$(int_env HMEM_SERVER_PORT 1 65535); export HMEM_SERVER_PORT
  HMEM_DB_PORT=$(int_env HMEM_DB_PORT 1 65535); export HMEM_DB_PORT
  HMEM_POOL_SIZE=$(int_env HMEM_POOL_SIZE 1 1000); export HMEM_POOL_SIZE
  HMEM_POOL_IDLE_TIMEOUT=$(decimal_env HMEM_POOL_IDLE_TIMEOUT 1 3600); export HMEM_POOL_IDLE_TIMEOUT
  HMEM_POOL_STATEMENT_TIMEOUT_MS=$(int_env HMEM_POOL_STATEMENT_TIMEOUT_MS 1000 300000); export HMEM_POOL_STATEMENT_TIMEOUT_MS
  HMEM_LOG_MAX_SIZE_MB=$(int_env HMEM_LOG_MAX_SIZE_MB 1 10000); export HMEM_LOG_MAX_SIZE_MB
  HMEM_LOG_BACKUP_COUNT=$(int_env HMEM_LOG_BACKUP_COUNT 0 100); export HMEM_LOG_BACKUP_COUNT
  HMEM_RATE_LIMIT_REQUESTS_PER_SECOND=$(decimal_env HMEM_RATE_LIMIT_REQUESTS_PER_SECOND 0.1 10000.0); export HMEM_RATE_LIMIT_REQUESTS_PER_SECOND
  HMEM_RATE_LIMIT_BURST=$(int_env HMEM_RATE_LIMIT_BURST 1 100000); export HMEM_RATE_LIMIT_BURST
  HMEM_AUTH_DEPLOYED_SESSION_TTL_SECONDS=$(int_env HMEM_AUTH_DEPLOYED_SESSION_TTL_SECONDS 60 315360000); export HMEM_AUTH_DEPLOYED_SESSION_TTL_SECONDS
  HMEM_DB_CONNECT_RETRIES=$(int_env HMEM_DB_CONNECT_RETRIES 1 3600); export HMEM_DB_CONNECT_RETRIES
  HMEM_DB_CONNECT_RETRY_DELAY_SECONDS=$(int_env HMEM_DB_CONNECT_RETRY_DELAY_SECONDS 0 3600); export HMEM_DB_CONNECT_RETRY_DELAY_SECONDS

  HMEM_WEB_ENABLED=$(bool_env HMEM_WEB_ENABLED); export HMEM_WEB_ENABLED
  HMEM_RATE_LIMIT_ENABLED=$(bool_env HMEM_RATE_LIMIT_ENABLED); export HMEM_RATE_LIMIT_ENABLED
  HMEM_AUTH_ENABLED=$(bool_env HMEM_AUTH_ENABLED); export HMEM_AUTH_ENABLED
  HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED=$(bool_env HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED); export HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED
  HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=$(bool_env HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP); export HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP
  HMEM_AUTH_DEPLOYED_COOKIE_SECURE=$(bool_env HMEM_AUTH_DEPLOYED_COOKIE_SECURE); export HMEM_AUTH_DEPLOYED_COOKIE_SECURE
  HMEM_FRONTEND_REQUIRE_AUTH_STATE=$(bool_env HMEM_FRONTEND_REQUIRE_AUTH_STATE); export HMEM_FRONTEND_REQUIRE_AUTH_STATE

  one_of HMEM_AUTH_MODE local deployed
  one_of HMEM_AUTH_DEPLOYED_TOKEN_LOOKUP database
  one_of HMEM_AUTH_DEPLOYED_COOKIE_SAME_SITE Lax Strict None
  one_of HMEM_FRONTEND_AUTH_TOKEN_STORAGE local session memory

  case "${HMEM_DB_SSLMODE}" in
    disable|allow|prefer|require|verify-ca|verify-full) : ;;
    *) fail "HMEM_DB_SSLMODE must be a valid libpq sslmode" ;;
  esac

  validate_nonempty HMEM_SERVER_HOST
  validate_nonempty HMEM_DB_HOST
  validate_nonempty HMEM_DB_NAME
  validate_nonempty HMEM_DB_USER

  for scalar_var in \
    HMEM_SERVER_HOST HMEM_DB_HOST HMEM_DB_NAME HMEM_DB_USER HMEM_DB_SSLMODE \
    HMEM_LOG_LEVEL HMEM_WEB_STATIC_DIR HMEM_AUTH_MODE HMEM_AUTH_DEPLOYED_TOKEN_LOOKUP \
    HMEM_AUTH_DEPLOYED_SESSION_COOKIE_NAME HMEM_AUTH_DEPLOYED_CSRF_COOKIE_NAME \
    HMEM_AUTH_DEPLOYED_CSRF_HEADER_NAME HMEM_AUTH_DEPLOYED_COOKIE_SAME_SITE \
    HMEM_FRONTEND_API_URL HMEM_FRONTEND_WS_URL HMEM_FRONTEND_AUTH_MODE \
    HMEM_FRONTEND_LOGIN_URL HMEM_FRONTEND_LOGOUT_URL HMEM_FRONTEND_LOGOUT_REDIRECT_URL \
    HMEM_FRONTEND_CSRF_COOKIE_NAME HMEM_FRONTEND_CSRF_HEADER_NAME \
    HMEM_FRONTEND_AUTH_TOKEN_STORAGE HMEM_FRONTEND_AUTH_TOKEN_STORAGE_KEY \
    HMEM_AUTH_DEPLOYED_ISSUER HMEM_AUTH_DEPLOYED_AUDIENCE HMEM_AUTH_DEPLOYED_DISCOVERY_URL \
    HMEM_AUTH_DEPLOYED_JWKS_URL HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET \
    HMEM_AUTH_DEPLOYED_CLIENT_ID HMEM_AUTH_DEPLOYED_CLIENT_SECRET \
    HMEM_AUTH_DEPLOYED_REDIRECT_URI HMEM_AUTH_DEPLOYED_AUTHORIZATION_ENDPOINT \
    HMEM_AUTH_DEPLOYED_TOKEN_ENDPOINT HMEM_TLS_CERT_FILE HMEM_TLS_KEY_FILE
  do
    validate_scalar_no_newline "$scalar_var"
  done
  validate_csv_scalars_no_newline HMEM_CORS_ALLOWED_ORIGINS
  validate_csv_scalars_no_newline HMEM_AUTH_DEPLOYED_SCOPES
  validate_csv_scalars_no_newline HMEM_FRONTEND_AUTH_TOKEN_URL_PARAMS

  if [ -n "$(get_env HMEM_TLS_CERT_FILE)" ] && [ -z "$(get_env HMEM_TLS_KEY_FILE)" ]; then
    fail "HMEM_TLS_CERT_FILE and HMEM_TLS_KEY_FILE must be set together"
  fi
  if [ -z "$(get_env HMEM_TLS_CERT_FILE)" ] && [ -n "$(get_env HMEM_TLS_KEY_FILE)" ]; then
    fail "HMEM_TLS_CERT_FILE and HMEM_TLS_KEY_FILE must be set together"
  fi

  if [ "$vc_mode" = "server" ] && [ "$HMEM_WEB_ENABLED" = "true" ]; then
    [ -d "$HMEM_WEB_STATIC_DIR" ] || fail "HMEM_WEB_ENABLED=true but HMEM_WEB_STATIC_DIR does not exist"
  fi
}

validate_startup_auth() {
  if [ "$HMEM_AUTH_MODE" = "local" ]; then
    if [ "$HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED" = "false" ]; then
      [ "$HMEM_AUTH_ENABLED" = "true" ] || fail "local static-bearer profile requires HMEM_AUTH_ENABLED=true"
      env_is_set_nonempty HMEM_API_KEY || fail "local static-bearer profile requires HMEM_API_KEY or HMEM_API_KEY_FILE"
    else
      if [ "$HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP" = "true" ]; then
        warn "DEV-ONLY: HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=true may expose the implicit local superadmin to remote clients"
      elif ! is_loopback_host "$HMEM_SERVER_HOST" || cors_has_remote_or_wildcard "$HMEM_CORS_ALLOWED_ORIGINS"; then
        fail "unsafe local bootstrap: bind to loopback/local CORS, disable bootstrap, or set HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP=true only for trusted development"
      fi
    fi
  fi

  if [ "$HMEM_AUTH_MODE" = "deployed" ]; then
    validate_nonempty HMEM_AUTH_DEPLOYED_ISSUER
    validate_nonempty HMEM_AUTH_DEPLOYED_AUDIENCE
    validate_https_url HMEM_AUTH_DEPLOYED_ISSUER
    validate_https_url HMEM_AUTH_DEPLOYED_DISCOVERY_URL
    validate_https_url HMEM_AUTH_DEPLOYED_JWKS_URL
    validate_https_url HMEM_AUTH_DEPLOYED_AUTHORIZATION_ENDPOINT
    validate_https_url HMEM_AUTH_DEPLOYED_TOKEN_ENDPOINT

    if [ -z "$(get_env HMEM_AUTH_DEPLOYED_DISCOVERY_URL)" ] \
      && [ -z "$(get_env HMEM_AUTH_DEPLOYED_JWKS_URL)" ] \
      && [ -z "$(get_env HMEM_AUTH_DEPLOYED_JWKS_FILE)" ]; then
      fail "deployed auth requires HMEM_AUTH_DEPLOYED_DISCOVERY_URL, HMEM_AUTH_DEPLOYED_JWKS_URL, or HMEM_AUTH_DEPLOYED_JWKS_FILE"
    fi

    if [ "$HMEM_WEB_ENABLED" = "true" ]; then
      if [ -z "$(get_env HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN)" ]; then
        HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN=true; export HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN
      fi
    fi
    default_env HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN "false"
    HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN=$(bool_env HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN); export HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN

    if [ "$HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN" = "true" ]; then
      validate_nonempty HMEM_AUTH_DEPLOYED_CLIENT_ID
      validate_nonempty HMEM_AUTH_DEPLOYED_CLIENT_SECRET
      validate_nonempty HMEM_AUTH_DEPLOYED_REDIRECT_URI
      if [ -z "$(get_env HMEM_AUTH_DEPLOYED_DISCOVERY_URL)" ] \
        && { [ -z "$(get_env HMEM_AUTH_DEPLOYED_AUTHORIZATION_ENDPOINT)" ] || [ -z "$(get_env HMEM_AUTH_DEPLOYED_TOKEN_ENDPOINT)" ]; }; then
        fail "browser OIDC requires discovery_url or both authorization_endpoint and token_endpoint"
      fi
      if cors_has_wildcard "$HMEM_CORS_ALLOWED_ORIGINS"; then
        fail "deployed browser cookie auth must not use wildcard CORS origins"
      fi
    fi

    csv_contains_token "$HMEM_AUTH_DEPLOYED_SCOPES" openid || fail "HMEM_AUTH_DEPLOYED_SCOPES must include openid"

    if [ "$HMEM_AUTH_DEPLOYED_COOKIE_SAME_SITE" = "None" ] && [ "$HMEM_AUTH_DEPLOYED_COOKIE_SECURE" != "true" ]; then
      fail "HMEM_AUTH_DEPLOYED_COOKIE_SAME_SITE=None requires HMEM_AUTH_DEPLOYED_COOKIE_SECURE=true"
    fi

    env_is_set_nonempty HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET || warn "HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET is unset; defer new PAT/service-token issuance until configured"
  fi

  if [ -z "$HMEM_CORS_ALLOWED_ORIGINS" ]; then
    warn "HMEM_CORS_ALLOWED_ORIGINS is empty; same-origin browsers work, cross-origin browsers do not"
  fi
}

passwd_home_for() {
  ph_user=$1
  if command -v getent >/dev/null 2>&1; then
    getent passwd "$ph_user" | awk -F: '{ print $6; exit }'
  elif [ -r /etc/passwd ]; then
    awk -F: -v user="$ph_user" '$1 == user { print $6; exit }' /etc/passwd
  fi
}

verify_home_contract() {
  [ "$HOME" = "$HMEM_HOME" ] || fail "HOME must be $HMEM_HOME"

  vh_current_uid=$(id -u)
  vh_user=${HMEM_RUNTIME_USER:-hmem}
  if [ "$vh_current_uid" = "0" ] && id "$vh_user" >/dev/null 2>&1; then
    vh_home=$(passwd_home_for "$vh_user")
    [ -z "$vh_home" ] || [ "$vh_home" = "$HMEM_HOME" ] || fail "passwd home for $vh_user must be $HMEM_HOME"
  else
    vh_name=$(id -un 2>/dev/null || true)
    if [ -n "$vh_name" ]; then
      vh_home=$(passwd_home_for "$vh_name")
      [ -z "$vh_home" ] || [ "$vh_home" = "$HMEM_HOME" ] || fail "passwd home for $vh_name must be $HMEM_HOME"
    fi
  fi
}

chown_runtime_paths() {
  [ "$(id -u)" = "0" ] || return 0
  cr_user=${HMEM_RUNTIME_USER:-hmem}
  id "$cr_user" >/dev/null 2>&1 || return 0
  cr_group=${HMEM_RUNTIME_GROUP:-}
  if [ -n "$cr_group" ]; then
    cr_owner="$cr_user:$cr_group"
  else
    cr_owner="$cr_user"
  fi
  chown "$cr_owner" "$HMEM_HOME" "$CONFIG_DIR" "$LOG_DIR" || fail "failed to chown runtime directories"
  [ ! -e "$CONFIG_FILE" ] || chown "$cr_owner" "$CONFIG_FILE" || fail "failed to chown $CONFIG_FILE"
}

prepare_directories() {
  verify_home_contract
  CONFIG_DIR="$HMEM_HOME/.hmem"
  LOG_DIR="$CONFIG_DIR/logs"
  CONFIG_FILE="$CONFIG_DIR/$CONFIG_BASENAME"
  export CONFIG_DIR LOG_DIR CONFIG_FILE

  mkdir -p "$CONFIG_DIR" "$LOG_DIR" || fail "failed to create hmem runtime directories"
  chmod 700 "$HMEM_HOME" "$CONFIG_DIR" "$LOG_DIR" || fail "failed to set hmem runtime directory permissions"
  [ -w "$HMEM_HOME" ] || fail "$HMEM_HOME is not writable"
  [ -w "$CONFIG_DIR" ] || fail "$CONFIG_DIR is not writable"
  [ -w "$LOG_DIR" ] || fail "$LOG_DIR is not writable"
  chown_runtime_paths
}

yaml_quote() {
  yq_value=$1
  case "$yq_value" in
    *"
"*) fail "YAML scalar values must not contain newlines" ;;
  esac
  printf "%s" "$yq_value" | sed "s/'/''/g; 1s/^/'/; \$s/\$/'/"
}

yaml_optional_scalar() {
  yos_indent=$1
  yos_key=$2
  yos_value=$3
  [ -n "$yos_value" ] || return 0
  printf '%s%s: %s\n' "$yos_indent" "$yos_key" "$(yaml_quote "$yos_value")"
}

yaml_csv_list_after_key() {
  ycl_indent=$1
  ycl_csv=$2
  ycl_seen=0
  if [ -n "$ycl_csv" ]; then
    ycl_oldifs=$IFS
    IFS=,
    set -- $ycl_csv
    IFS=$ycl_oldifs
    for ycl_item in "$@"; do
      ycl_item=$(trim "$ycl_item")
      [ -n "$ycl_item" ] || continue
      [ "$ycl_seen" = "0" ] && printf '\n'
      ycl_seen=1
      printf '%s- %s\n' "$ycl_indent" "$(yaml_quote "$ycl_item")"
    done
  fi
  [ "$ycl_seen" = "1" ] || printf ' []\n'
}

write_indented_file_value() {
  wif_file=$1
  wif_indent=$2
  [ -f "$wif_file" ] || fail "file does not exist: $wif_file"
  [ -r "$wif_file" ] || fail "file is unreadable: $wif_file"
  [ -s "$wif_file" ] || fail "file is empty: $wif_file"
  sed "s/^/$wif_indent/" "$wif_file"
}

generate_config() {
  gc_tmp="$CONFIG_FILE.tmp.$$"
  umask 077
  {
    printf 'server:\n'
    printf '  host: %s\n' "$(yaml_quote "$HMEM_SERVER_HOST")"
    printf '  port: %s\n' "$HMEM_SERVER_PORT"

    printf 'database:\n'
    printf '  host: %s\n' "$(yaml_quote "$HMEM_DB_HOST")"
    printf '  port: %s\n' "$HMEM_DB_PORT"
    printf '  name: %s\n' "$(yaml_quote "$HMEM_DB_NAME")"
    printf '  user: %s\n' "$(yaml_quote "$HMEM_DB_USER")"

    printf 'pool:\n'
    printf '  size: %s\n' "$HMEM_POOL_SIZE"
    printf '  idle_timeout: %s\n' "$HMEM_POOL_IDLE_TIMEOUT"
    printf '  statement_timeout_ms: %s\n' "$HMEM_POOL_STATEMENT_TIMEOUT_MS"

    printf 'logging:\n'
    printf '  level: %s\n' "$(yaml_quote "$HMEM_LOG_LEVEL")"
    printf '  max_size_mb: %s\n' "$HMEM_LOG_MAX_SIZE_MB"
    printf '  backup_count: %s\n' "$HMEM_LOG_BACKUP_COUNT"

    printf 'cors:\n'
    printf '  allowed_origins:'
    yaml_csv_list_after_key '    ' "$HMEM_CORS_ALLOWED_ORIGINS"

    printf 'auth:\n'
    printf '  mode: %s\n' "$(yaml_quote "$HMEM_AUTH_MODE")"
    printf '  enabled: %s\n' "$HMEM_AUTH_ENABLED"
    printf '  local:\n'
    printf '    bootstrap_enabled: %s\n' "$HMEM_AUTH_LOCAL_BOOTSTRAP_ENABLED"
    printf '    allow_remote_bootstrap: %s\n' "$HMEM_AUTH_LOCAL_ALLOW_REMOTE_BOOTSTRAP"
    printf '    bot_tokens:'
    if [ -n "$(get_env HMEM_AUTH_LOCAL_BOT_TOKENS_FILE)" ]; then
      printf '\n'
      write_indented_file_value "$HMEM_AUTH_LOCAL_BOT_TOKENS_FILE" '      '
    else
      printf ' []\n'
    fi
    printf '  deployed:\n'
    yaml_optional_scalar '    ' issuer "$(get_env HMEM_AUTH_DEPLOYED_ISSUER)"
    yaml_optional_scalar '    ' audience "$(get_env HMEM_AUTH_DEPLOYED_AUDIENCE)"
    yaml_optional_scalar '    ' discovery_url "$(get_env HMEM_AUTH_DEPLOYED_DISCOVERY_URL)"
    yaml_optional_scalar '    ' jwks_url "$(get_env HMEM_AUTH_DEPLOYED_JWKS_URL)"
    if [ -n "$(get_env HMEM_AUTH_DEPLOYED_JWKS_FILE)" ]; then
      printf '    jwks:\n'
      write_indented_file_value "$HMEM_AUTH_DEPLOYED_JWKS_FILE" '      '
    fi
    printf '    token_lookup: %s\n' "$(yaml_quote "$HMEM_AUTH_DEPLOYED_TOKEN_LOOKUP")"
    yaml_optional_scalar '    ' token_hash_secret "$(get_env HMEM_AUTH_DEPLOYED_TOKEN_HASH_SECRET)"
    yaml_optional_scalar '    ' client_id "$(get_env HMEM_AUTH_DEPLOYED_CLIENT_ID)"
    yaml_optional_scalar '    ' client_secret "$(get_env HMEM_AUTH_DEPLOYED_CLIENT_SECRET)"
    yaml_optional_scalar '    ' redirect_uri "$(get_env HMEM_AUTH_DEPLOYED_REDIRECT_URI)"
    printf '    scopes:'
    yaml_csv_list_after_key '      ' "$HMEM_AUTH_DEPLOYED_SCOPES"
    yaml_optional_scalar '    ' authorization_endpoint "$(get_env HMEM_AUTH_DEPLOYED_AUTHORIZATION_ENDPOINT)"
    yaml_optional_scalar '    ' token_endpoint "$(get_env HMEM_AUTH_DEPLOYED_TOKEN_ENDPOINT)"
    printf '    session_cookie_name: %s\n' "$(yaml_quote "$HMEM_AUTH_DEPLOYED_SESSION_COOKIE_NAME")"
    printf '    csrf_cookie_name: %s\n' "$(yaml_quote "$HMEM_AUTH_DEPLOYED_CSRF_COOKIE_NAME")"
    printf '    csrf_header_name: %s\n' "$(yaml_quote "$HMEM_AUTH_DEPLOYED_CSRF_HEADER_NAME")"
    printf '    session_ttl_seconds: %s\n' "$HMEM_AUTH_DEPLOYED_SESSION_TTL_SECONDS"
    printf '    cookie_secure: %s\n' "$HMEM_AUTH_DEPLOYED_COOKIE_SECURE"
    printf '    cookie_same_site: %s\n' "$(yaml_quote "$HMEM_AUTH_DEPLOYED_COOKIE_SAME_SITE")"

    printf 'rate_limit:\n'
    printf '  enabled: %s\n' "$HMEM_RATE_LIMIT_ENABLED"
    printf '  requests_per_second: %s\n' "$HMEM_RATE_LIMIT_REQUESTS_PER_SECOND"
    printf '  burst: %s\n' "$HMEM_RATE_LIMIT_BURST"

    if [ -n "$(get_env HMEM_TLS_CERT_FILE)" ] || [ -n "$(get_env HMEM_TLS_KEY_FILE)" ]; then
      printf 'tls:\n'
      yaml_optional_scalar '  ' cert_file "$(get_env HMEM_TLS_CERT_FILE)"
      yaml_optional_scalar '  ' key_file "$(get_env HMEM_TLS_KEY_FILE)"
    else
      printf 'tls: {}\n'
    fi

    printf 'web:\n'
    printf '  enabled: %s\n' "$HMEM_WEB_ENABLED"
    if [ -n "$HMEM_WEB_STATIC_DIR" ]; then
      printf '  static_dir: %s\n' "$(yaml_quote "$HMEM_WEB_STATIC_DIR")"
    fi
  } > "$gc_tmp" || fail "failed to write generated config"
  chmod 600 "$gc_tmp" || fail "failed to chmod generated config"
  mv "$gc_tmp" "$CONFIG_FILE" || fail "failed to install generated config"
  chown_runtime_paths
  log "Generated $CONFIG_FILE"
}

json_quote() {
  jq_value=$1
  case "$jq_value" in
    *"
"*) fail "frontend runtime config values must not contain newlines" ;;
  esac
  printf '%s' "$jq_value" | sed 's/\\/\\\\/g; s/"/\\"/g; 1s/^/"/; $s/$/"/'
}

js_begin_object() {
  js_out=$1
  printf '(function () {\n  window.HMEM_CONFIG = {\n' > "$js_out"
  JS_FIRST=1
}

js_add_raw() {
  js_out=$1
  js_key=$2
  js_raw=$3
  if [ "$JS_FIRST" = "0" ]; then
    printf ',\n' >> "$js_out"
  fi
  JS_FIRST=0
  printf '    "%s": %s' "$js_key" "$js_raw" >> "$js_out"
}

js_add_string() {
  ja_out=$1
  ja_key=$2
  ja_value=$3
  js_add_raw "$ja_out" "$ja_key" "$(json_quote "$ja_value")"
}

js_add_optional_string() {
  ja_out=$1
  ja_key=$2
  ja_value=$3
  [ -n "$ja_value" ] || return 0
  js_add_string "$ja_out" "$ja_key" "$ja_value"
}

js_add_bool() {
  ja_out=$1
  ja_key=$2
  ja_value=$3
  js_add_raw "$ja_out" "$ja_key" "$ja_value"
}

js_add_csv_array() {
  ja_out=$1
  ja_key=$2
  ja_csv=$3
  ja_json='['
  ja_seen=0
  ja_oldifs=$IFS
  IFS=,
  set -- $ja_csv
  IFS=$ja_oldifs
  for ja_item in "$@"; do
    ja_item=$(trim "$ja_item")
    [ -n "$ja_item" ] || continue
    if [ "$ja_seen" = "1" ]; then
      ja_json="$ja_json, "
    fi
    ja_seen=1
    ja_json="$ja_json$(json_quote "$ja_item")"
  done
  ja_json="$ja_json]"
  js_add_raw "$ja_out" "$ja_key" "$ja_json"
}

frontend_auth_mode() {
  fam_value=$(get_env HMEM_FRONTEND_AUTH_MODE)
  if [ -n "$fam_value" ]; then
    printf '%s' "$fam_value"
  else
    printf '%s' "$HMEM_AUTH_MODE"
  fi
}

generate_frontend_config() {
  [ "$HMEM_WEB_ENABLED" = "true" ] || return 0
  [ -d "$HMEM_WEB_STATIC_DIR" ] || fail "cannot generate frontend runtime config; static dir missing"
  [ -w "$HMEM_WEB_STATIC_DIR" ] || fail "cannot generate frontend runtime config; static dir is not writable"

  gf_file="$HMEM_WEB_STATIC_DIR/hmem-runtime-config.js"
  gf_tmp="$gf_file.tmp.$$"
  gf_auth_mode=$(frontend_auth_mode)

  if [ -z "$(get_env HMEM_FRONTEND_LOGIN_URL)" ] && [ "$gf_auth_mode" = "deployed" ]; then
    HMEM_FRONTEND_LOGIN_URL="/api/v1/auth/login"; export HMEM_FRONTEND_LOGIN_URL
  fi
  if [ -z "$(get_env HMEM_FRONTEND_LOGOUT_URL)" ] && [ "$gf_auth_mode" = "deployed" ]; then
    HMEM_FRONTEND_LOGOUT_URL="/api/v1/auth/logout"; export HMEM_FRONTEND_LOGOUT_URL
  fi
  if [ -z "$(get_env HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN)" ]; then
    if [ "$gf_auth_mode" = "deployed" ]; then
      HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN=true
    else
      HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN=false
    fi
    export HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN
  fi
  HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN=$(bool_env HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN); export HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN
  if [ -z "$(get_env HMEM_FRONTEND_CSRF_COOKIE_NAME)" ]; then
    HMEM_FRONTEND_CSRF_COOKIE_NAME=$HMEM_AUTH_DEPLOYED_CSRF_COOKIE_NAME; export HMEM_FRONTEND_CSRF_COOKIE_NAME
  fi
  if [ -z "$(get_env HMEM_FRONTEND_CSRF_HEADER_NAME)" ]; then
    HMEM_FRONTEND_CSRF_HEADER_NAME=$HMEM_AUTH_DEPLOYED_CSRF_HEADER_NAME; export HMEM_FRONTEND_CSRF_HEADER_NAME
  fi

  js_begin_object "$gf_tmp"
  js_add_optional_string "$gf_tmp" apiUrl "$(get_env HMEM_FRONTEND_API_URL)"
  js_add_optional_string "$gf_tmp" wsUrl "$(get_env HMEM_FRONTEND_WS_URL)"
  js_add_string "$gf_tmp" authMode "$gf_auth_mode"
  js_add_string "$gf_tmp" runtimeMode "$gf_auth_mode"
  js_add_optional_string "$gf_tmp" loginUrl "$(get_env HMEM_FRONTEND_LOGIN_URL)"
  js_add_optional_string "$gf_tmp" logoutUrl "$(get_env HMEM_FRONTEND_LOGOUT_URL)"
  js_add_string "$gf_tmp" logoutRedirectUrl "$HMEM_FRONTEND_LOGOUT_REDIRECT_URL"
  js_add_bool "$gf_tmp" serverSideOidcLogin "$HMEM_FRONTEND_SERVER_SIDE_OIDC_LOGIN"
  js_add_string "$gf_tmp" csrfCookieName "$HMEM_FRONTEND_CSRF_COOKIE_NAME"
  js_add_string "$gf_tmp" csrfHeaderName "$HMEM_FRONTEND_CSRF_HEADER_NAME"
  js_add_string "$gf_tmp" authTokenStorage "$HMEM_FRONTEND_AUTH_TOKEN_STORAGE"
  js_add_string "$gf_tmp" authTokenStorageKey "$HMEM_FRONTEND_AUTH_TOKEN_STORAGE_KEY"
  js_add_csv_array "$gf_tmp" authTokenUrlParams "$HMEM_FRONTEND_AUTH_TOKEN_URL_PARAMS"
  js_add_bool "$gf_tmp" requireAuthState "$HMEM_FRONTEND_REQUIRE_AUTH_STATE"
  printf '\n  };\n})();\n' >> "$gf_tmp"

  chmod 644 "$gf_tmp" || fail "failed to chmod frontend runtime config"
  mv "$gf_tmp" "$gf_file" || fail "failed to install frontend runtime config"
  log "Generated $gf_file"
}

prepare_runtime() {
  pr_mode=$1
  load_secret_files
  apply_defaults
  validate_common_config "$pr_mode"
  if [ "$pr_mode" = "server" ]; then
    validate_startup_auth
  fi
  prepare_directories
  generate_config
  if [ "$pr_mode" = "server" ]; then
    generate_frontend_config
  fi
}

run_migrations_with_retries() {
  [ -d "$HMEM_MIGRATIONS_DIR" ] || fail "migrations directory does not exist: $HMEM_MIGRATIONS_DIR"
  set +f
  set -- "$HMEM_MIGRATIONS_DIR"/V*.sql
  set -f
  [ -e "$1" ] || fail "migrations directory contains no V*.sql files: $HMEM_MIGRATIONS_DIR"
  rm_attempt=1
  while :; do
    log "Running database migrations (attempt $rm_attempt/$HMEM_DB_CONNECT_RETRIES)"
    if hmem-ctl migrate --migrations-dir "$HMEM_MIGRATIONS_DIR"; then
      log "Database migrations complete"
      return 0
    fi
    if [ "$rm_attempt" -ge "$HMEM_DB_CONNECT_RETRIES" ]; then
      fail "PostgreSQL did not become ready or migrations failed after $HMEM_DB_CONNECT_RETRIES attempt(s)"
    fi
    log "PostgreSQL unavailable or migrations failed; retrying in $HMEM_DB_CONNECT_RETRY_DELAY_SECONDS second(s)"
    sleep "$HMEM_DB_CONNECT_RETRY_DELAY_SECONDS"
    rm_attempt=$((rm_attempt + 1))
  done
}

prepare_mcp_environment() {
  load_mcp_secret_files
  apply_defaults
  HMEM_SERVER_PORT=$(int_env HMEM_SERVER_PORT 1 65535); export HMEM_SERVER_PORT
  default_env HMEM_SERVER_URL "http://127.0.0.1:${HMEM_SERVER_PORT}"
}

prepare_debug_environment() {
  apply_defaults
  HMEM_SERVER_PORT=$(int_env HMEM_SERVER_PORT 1 65535); export HMEM_SERVER_PORT
  default_env HMEM_SERVER_URL "http://127.0.0.1:${HMEM_SERVER_PORT}"
}

exec_as_runtime_user() {
  if [ "$(id -u)" = "0" ]; then
    ea_user=${HMEM_RUNTIME_USER:-hmem}
    if id "$ea_user" >/dev/null 2>&1; then
      if command -v gosu >/dev/null 2>&1; then
        exec gosu "$ea_user" "$@"
      fi
      if command -v su-exec >/dev/null 2>&1; then
        exec su-exec "$ea_user" "$@"
      fi
      warn "running as root because gosu/su-exec is unavailable; install one or set USER to the runtime user"
    fi
  fi
  exec "$@"
}

main() {
  if [ "$#" -eq 0 ]; then
    set -- hmem-server
  fi

  case "$1" in
    hmem-server|server)
      if [ "$1" = "server" ]; then
        shift
        set -- hmem-server "$@"
      fi
      prepare_runtime server
      run_migrations_with_retries
      log "Starting hmem-server"
      exec_as_runtime_user "$@"
      ;;
    migrate)
      shift
      prepare_runtime migrate
      run_migrations_with_retries
      ;;
    hmem-ctl)
      prepare_runtime ctl
      exec_as_runtime_user "$@"
      ;;
    hmem-mcp)
      prepare_mcp_environment
      exec_as_runtime_user "$@"
      ;;
    sh|/bin/sh|bash|/bin/bash|ash|/bin/ash|sleep|env)
      prepare_debug_environment
      exec_as_runtime_user "$@"
      ;;
    *)
      exec_as_runtime_user "$@"
      ;;
  esac
}

main "$@"
