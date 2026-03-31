# Security

This document describes hmem's security features, threat model, and hardening guidance.

## Threat Model

hmem is designed as a **local-first** service: by default it listens on `localhost:8420` with no authentication, no TLS, and a local PostgreSQL instance. This is appropriate for single-user developer workstations.

When deployed on a network — shared server, cloud VM, or across containers — you should enable the optional security layers described below.

### Trust Boundaries

```
Untrusted ─► [Network] ─► hmem-server ─► PostgreSQL
                              │
              hmem-mcp ───────┘ (HTTP, localhost or remote)
```

- **hmem-mcp → hmem-server**: HTTP calls. Trusted by default on localhost; secure with auth + TLS when remote.
- **hmem-server → PostgreSQL**: SQL over TCP. Trusted by default on localhost socket; secure with SSL when remote.
- **External clients → hmem-server**: Only relevant if the server port is exposed. Protect with auth, TLS, CORS, and rate limiting.

## Authentication

Authentication is **optional** and **disabled by default**.

### Enabling Bearer Token Auth

In `~/.hmem/config.yaml`:

```yaml
auth:
  enabled: true
  api_key: "your-secret-token"
```

Or via environment variable (takes precedence over config file):

```bash
export HMEM_API_KEY="your-secret-token"
```

When enabled, all requests (except `OPTIONS`) must include:

```
Authorization: Bearer your-secret-token
```

Unauthenticated requests receive `401 Unauthorized`.

### Safety Behavior

If `auth.enabled` is `true` but no API key is configured (neither in the config file nor via `HMEM_API_KEY`), the server **auto-disables** authentication and logs a warning. This prevents accidental lockout.

### hmem-mcp Forwarding

When `hmem-mcp` is configured with `--api-key`, it automatically sends the `Authorization: Bearer` header with every request to `hmem-server`.

## TLS / HTTPS

TLS is **optional**. When both a certificate and private key are provided, the server runs HTTPS via `warp-tls`. Otherwise it runs plain HTTP.

### Configuration

In `~/.hmem/config.yaml`:

```yaml
tls:
  cert_file: /path/to/cert.pem
  key_file: /path/to/key.pem
```

Or via CLI flags (override config file):

```bash
hmem-server --tls-cert /path/to/cert.pem --tls-key /path/to/key.pem
```

### Incomplete TLS Config

If only one of cert/key is provided, the server logs a warning and falls back to plain HTTP. It will **not** fail to start.

### Recommendation

For production on a network:
- Use a reverse proxy (nginx, Caddy) that terminates TLS, or
- Provide a valid certificate/key pair directly to hmem-server

Self-signed certificates work for internal deployments. For public-facing setups, use certificates from a trusted CA (e.g., Let's Encrypt).

## Database Security

### Remote Database Connection

hmem supports connecting to a remote PostgreSQL instance. Configure the connection in `~/.hmem/config.yaml`:

```yaml
database:
  host: pg.example.com
  port: 5432
  name: hmem
  user: hmem_app
  password: ~          # Use HMEM_DB_PASSWORD env var instead
  sslmode: require     # Or: verify-ca, verify-full
```

### SSL Modes

The `sslmode` field accepts standard PostgreSQL values:

| Mode | Description |
|------|-------------|
| `disable` | No SSL |
| `allow` | SSL if server supports it |
| `prefer` | SSL preferred (PostgreSQL default) |
| `require` | SSL mandatory, no certificate verification |
| `verify-ca` | SSL mandatory, verify server certificate against CA |
| `verify-full` | SSL mandatory, verify CA and hostname match |

Set via config or environment variable:

```bash
export HMEM_DB_SSLMODE="verify-full"
```

### Secrets Handling

**Never commit database passwords or API keys to version control.**

Use environment variables for sensitive values:

| Variable | Purpose |
|----------|---------|
| `HMEM_DB_PASSWORD` | Database password |
| `HMEM_API_KEY` | Bearer token for API authentication |
| `HMEM_DB_SSLMODE` | PostgreSQL SSL mode |

Environment variables take precedence over `~/.hmem/config.yaml` values. This enables secure deployment patterns:
- Inject secrets via systemd `EnvironmentFile=`
- Use container orchestrator secrets (Docker secrets, Kubernetes secrets)
- Use a `.env` file excluded from version control

## CORS

CORS is enforced on all requests when an `Origin` header is present.

### Default Origins

```yaml
cors:
  allowed_origins:
    - "http://localhost"
    - "http://127.0.0.1"
```

The default allows localhost on any port (port-wildcard matching). For example, `http://localhost:3000` matches the `http://localhost` pattern.

### Wildcard

Setting `"*"` allows all origins:

```yaml
cors:
  allowed_origins:
    - "*"
```

Use this only in trusted environments or behind a reverse proxy with its own CORS policy.

### Allowed Methods and Headers

- **Methods**: GET, POST, PUT, DELETE, OPTIONS
- **Request Headers**: Content-Type, Authorization

## Rate Limiting

Rate limiting is **optional** and **disabled by default**.

### Configuration

```yaml
rate_limit:
  enabled: true
  requests_per_second: 10.0
  burst: 20
```

Uses a **token bucket** algorithm per client IP (or `X-Forwarded-For` if behind a proxy). Requests exceeding the limit receive `429 Too Many Requests` with a `Retry-After: 1` header.

### Validation Bounds

- `requests_per_second`: 0.1 – 10,000
- `burst`: 1 – 100,000

Values outside these ranges are clamped with a warning at startup.

## Connection Pool

### Statement Timeout

A configurable statement timeout prevents runaway queries:

```yaml
pool:
  statement_timeout_ms: 30000   # 30 seconds (default)
```

### Pool Metrics

The `/health` endpoint reports pool utilization. A warning is logged when active connections exceed 80% of the pool maximum.

## Hardening Checklist

For network-exposed deployments:

- [ ] Set `auth.enabled: true` with a strong `HMEM_API_KEY`
- [ ] Enable TLS (direct or via reverse proxy)
- [ ] Set `database.sslmode` to `require` or stricter for remote databases
- [ ] Use `HMEM_DB_PASSWORD` environment variable instead of config file
- [ ] Restrict `cors.allowed_origins` to actual client origins
- [ ] Enable `rate_limit` if exposed to untrusted clients
- [ ] Bind to `127.0.0.1` if only local clients need access (default)
- [ ] Run PostgreSQL with authentication enabled (`pg_hba.conf`)
- [ ] Keep hmem and PostgreSQL updated

## Reporting Vulnerabilities

If you discover a security issue, please report it privately rather than opening a public issue. Contact the maintainer directly.
