# syntax=docker/dockerfile:1.7

ARG NODE_IMAGE=node:20-bookworm-slim
ARG HASKELL_IMAGE=haskell:9.10.2-slim
ARG RUNTIME_IMAGE=debian:12-slim

FROM ${NODE_IMAGE} AS frontend-builder
WORKDIR /workspace/hmem-server/frontend

COPY hmem-server/frontend/package.json hmem-server/frontend/package-lock.json hmem-server/frontend/elm.json ./
RUN npm ci

COPY hmem-server/frontend/index.html hmem-server/frontend/vite.config.js ./
COPY hmem-server/frontend/src ./src
RUN npm run build

FROM ${HASKELL_IMAGE} AS haskell-builder
WORKDIR /workspace
ENV STACK_ROOT=/root/.stack

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      libffi-dev \
      libgmp-dev \
      libpq-dev \
      libtinfo-dev \
      pkg-config \
      xz-utils \
      zlib1g-dev; \
    rm -rf /var/lib/apt/lists/*

COPY stack.yaml stack.yaml.lock ./
COPY hmem-core/package.yaml hmem-core/hmem-core.cabal hmem-core/
COPY hmem-server/package.yaml hmem-server/hmem-server.cabal hmem-server/
COPY hmem-mcp/package.yaml hmem-mcp/hmem-mcp.cabal hmem-mcp/

RUN stack --version; \
    stack --system-ghc --no-install-ghc build --only-dependencies \
      hmem-server:exe:hmem-server \
      hmem-server:exe:hmem-ctl \
      hmem-mcp:exe:hmem-mcp

COPY hmem-core/src hmem-core/src
COPY hmem-server/src hmem-server/src
COPY hmem-server/app hmem-server/app
COPY hmem-server/app-build hmem-server/app-build
COPY hmem-server/setup hmem-server/setup
COPY hmem-server/migrations hmem-server/migrations
COPY hmem-mcp/src hmem-mcp/src
COPY hmem-mcp/app hmem-mcp/app

RUN set -eux; \
    mkdir -p /opt/hmem/bin; \
    stack --system-ghc --no-install-ghc install \
      --local-bin-path /opt/hmem/bin \
      hmem-server:exe:hmem-server \
      hmem-server:exe:hmem-ctl \
      hmem-mcp:exe:hmem-mcp; \
    strip /opt/hmem/bin/hmem-server /opt/hmem/bin/hmem-ctl /opt/hmem/bin/hmem-mcp

FROM ${RUNTIME_IMAGE} AS runtime

ENV HMEM_HOME=/var/lib/hmem \
    HOME=/var/lib/hmem \
    HMEM_RUNTIME_USER=hmem \
    HMEM_RUNTIME_GROUP=hmem \
    HMEM_SERVER_PORT=8420 \
    HMEM_WEB_STATIC_DIR=/opt/hmem/static \
    HMEM_MIGRATIONS_DIR=/opt/hmem/migrations

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      grep \
      jq \
      libffi8 \
      libgmp10 \
      libncursesw6 \
      libnuma1 \
      libpq5 \
      libtinfo6 \
      zlib1g; \
    rm -rf /var/lib/apt/lists/*; \
    groupadd --system --gid 10001 hmem; \
    useradd --uid 10001 --gid hmem --home-dir /var/lib/hmem --create-home --shell /usr/sbin/nologin --no-log-init hmem; \
    mkdir -p /opt/hmem/static /opt/hmem/migrations /var/lib/hmem/.hmem/logs; \
    chown -R hmem:hmem /opt/hmem /var/lib/hmem; \
    chmod 700 /var/lib/hmem /var/lib/hmem/.hmem /var/lib/hmem/.hmem/logs

COPY --from=haskell-builder /opt/hmem/bin/hmem-server /usr/local/bin/hmem-server
COPY --from=haskell-builder /opt/hmem/bin/hmem-ctl /usr/local/bin/hmem-ctl
COPY --from=haskell-builder /opt/hmem/bin/hmem-mcp /usr/local/bin/hmem-mcp
COPY --from=frontend-builder --chown=hmem:hmem /workspace/hmem-server/static /opt/hmem/static
COPY --chown=hmem:hmem hmem-server/migrations /opt/hmem/migrations
COPY --chmod=0755 docker/entrypoint.sh /usr/local/bin/hmem-entrypoint
COPY --chmod=0755 docker/healthcheck.sh /usr/local/bin/hmem-healthcheck

RUN set -eux; \
    for bin in /usr/local/bin/hmem-server /usr/local/bin/hmem-ctl /usr/local/bin/hmem-mcp; do \
      ldd "$bin"; \
      if ldd "$bin" | grep 'not found'; then \
        exit 1; \
      fi; \
    done; \
    test -f /opt/hmem/static/index.html; \
    set -- /opt/hmem/migrations/V*.sql; \
    test -f "$1"

EXPOSE 8420
USER hmem:hmem
ENTRYPOINT ["/usr/local/bin/hmem-entrypoint"]
CMD ["hmem-server"]
HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 CMD ["/usr/local/bin/hmem-healthcheck"]
