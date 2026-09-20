# syntax=docker/dockerfile:1.7

ARG NODE_IMAGE=node:20-bookworm-slim
ARG HASKELL_BUILDER_IMAGE=debian:12-slim
ARG RUNTIME_IMAGE=debian:12-slim
ARG GPU_RUNTIME_IMAGE=ghcr.io/huggingface/text-embeddings-inference@sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170
ARG PG_LIB_IMAGE=pgvector/pgvector@sha256:cf134a767f474095eeba57e0117be8e568e011a63f33fbf252f14c9b760f8e6f

FROM ${NODE_IMAGE} AS frontend-builder
WORKDIR /workspace/hmem-server/frontend

ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends ca-certificates; \
    update-ca-certificates; \
    rm -rf /var/lib/apt/lists/*

COPY hmem-server/frontend/package.json hmem-server/frontend/package-lock.json hmem-server/frontend/elm.json ./
RUN npm ci

COPY hmem-server/frontend/index.html hmem-server/frontend/vite.config.js ./
COPY hmem-server/frontend/src ./src
RUN npm run build

FROM ${HASKELL_BUILDER_IMAGE} AS haskell-builder
WORKDIR /workspace
ENV STACK_ROOT=/root/.stack

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      curl \
      libffi-dev \
      libgmp-dev \
      libnuma-dev \
      libpq-dev \
      libtinfo-dev \
      pkg-config \
      xz-utils \
      zlib1g-dev; \
    curl -fsSL https://get.haskellstack.org/ -o /tmp/install-stack.sh; \
    sh /tmp/install-stack.sh; \
    rm /tmp/install-stack.sh; \
    stack --version; \
    rm -rf /var/lib/apt/lists/*

COPY stack.yaml stack.yaml.lock ./
COPY hmem-core/package.yaml hmem-core/hmem-core.cabal hmem-core/
COPY hmem-server/package.yaml hmem-server/hmem-server.cabal hmem-server/
COPY hmem-mcp/package.yaml hmem-mcp/hmem-mcp.cabal hmem-mcp/
COPY hmem-embedding-http/package.yaml hmem-embedding-http/hmem-embedding-http.cabal hmem-embedding-http/

RUN set -eux; \
    stack --no-terminal setup; \
    stack --no-terminal build --only-dependencies \
      hmem-server:exe:hmem-server \
      hmem-server:exe:hmem-ctl \
      hmem-mcp:exe:hmem-mcp \
      hmem-embedding-http:exe:hmem-embedding-http-helper

COPY hmem-core/src hmem-core/src
COPY hmem-server/src hmem-server/src
COPY hmem-server/app hmem-server/app
COPY hmem-server/app-build hmem-server/app-build
COPY hmem-server/setup hmem-server/setup
COPY hmem-server/migrations hmem-server/migrations
COPY hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json hmem-server/test/fixtures/embedding-gpu-viability/reference-golden-v1.json hmem-server/test/fixtures/embedding-gpu-viability/
COPY config/managed-embedding-provenance.yaml config/managed-embedding-provenance.yaml
COPY hmem-mcp/src hmem-mcp/src
COPY hmem-mcp/app hmem-mcp/app
COPY hmem-embedding-http/src hmem-embedding-http/src
COPY hmem-embedding-http/app hmem-embedding-http/app
COPY hmem-embedding-http/cbits hmem-embedding-http/cbits

RUN set -eux; \
    mkdir -p /opt/hmem/bin; \
    stack --no-terminal install \
      --local-bin-path /opt/hmem/bin \
      hmem-server:exe:hmem-server \
      hmem-server:exe:hmem-ctl \
      hmem-mcp:exe:hmem-mcp \
      hmem-embedding-http:exe:hmem-embedding-http-helper; \
    strip /opt/hmem/bin/hmem-server /opt/hmem/bin/hmem-ctl /opt/hmem/bin/hmem-mcp /opt/hmem/bin/hmem-embedding-http-helper

FROM ${PG_LIB_IMAGE} AS pg-runtime-libs
RUN set -eux; \
    mkdir -p /opt/hmem/pg-libs /opt/hmem/pg-licenses; \
    for lib in libpq.so.5 libldap-2.5.so.0 liblber-2.5.so.0; do \
      cp -L "/usr/lib/x86_64-linux-gnu/$lib" "/opt/hmem/pg-libs/$lib"; \
    done; \
    dpkg-query -W -f='${Package} ${Version}\n' libpq5 libldap-2.5-0 > /opt/hmem/pg-licenses/packages.txt; \
    for pkg in libpq5 libldap-2.5-0; do \
      cp "/usr/share/doc/$pkg/copyright" "/opt/hmem/pg-licenses/$pkg.copyright"; \
    done

FROM ${RUNTIME_IMAGE} AS runtime-base

ENV HMEM_HOME=/var/lib/hmem \
    HOME=/var/lib/hmem \
    HMEM_RUNTIME_USER=hmem \
    HMEM_RUNTIME_GROUP=hmem \
    HMEM_SERVER_PORT=8420 \
    HMEM_WEB_STATIC_DIR=/run/hmem/static \
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
    mkdir -p /opt/hmem/static /opt/hmem/migrations /opt/hmem/provenance /opt/hmem/licenses /var/lib/hmem/.hmem/logs; \
    chown -R hmem:hmem /opt/hmem /var/lib/hmem; \
    chmod 700 /var/lib/hmem /var/lib/hmem/.hmem /var/lib/hmem/.hmem/logs

COPY --from=haskell-builder /opt/hmem/bin/hmem-server /usr/local/bin/hmem-server
COPY --from=haskell-builder /opt/hmem/bin/hmem-ctl /usr/local/bin/hmem-ctl
COPY --from=haskell-builder /opt/hmem/bin/hmem-mcp /usr/local/bin/hmem-mcp
COPY --from=haskell-builder /opt/hmem/bin/hmem-embedding-http-helper /usr/local/bin/hmem-embedding-http-helper
COPY --from=frontend-builder --chown=hmem:hmem /workspace/hmem-server/static /opt/hmem/static
COPY --chown=hmem:hmem hmem-server/migrations /opt/hmem/migrations
COPY --chmod=0755 docker/entrypoint.sh /usr/local/bin/hmem-entrypoint
COPY --chmod=0755 docker/healthcheck.sh /usr/local/bin/hmem-healthcheck
COPY LICENSE /opt/hmem/licenses/hmem-LICENSE

RUN set -eux; \
    for bin in /usr/local/bin/hmem-server /usr/local/bin/hmem-ctl /usr/local/bin/hmem-mcp /usr/local/bin/hmem-embedding-http-helper; do \
      ldd "$bin"; \
      if ldd "$bin" | grep 'not found'; then \
        exit 1; \
      fi; \
      sha256sum "$bin"; \
    done > /opt/hmem/provenance/executable-and-abi.txt; \
    dpkg-query -W -f='${Package} ${Version}\n' > /opt/hmem/provenance/image-packages.txt; \
    test -f /opt/hmem/static/index.html; \
    set -- /opt/hmem/migrations/V*.sql; \
    test -f "$1"

FROM ${GPU_RUNTIME_IMAGE} AS gpu-runtime
USER root
ENV HMEM_HOME=/var/lib/hmem \
    HOME=/var/lib/hmem \
    HMEM_RUNTIME_USER=hmem \
    HMEM_RUNTIME_GROUP=hmem \
    HMEM_SERVER_PORT=8420 \
    HMEM_WEB_STATIC_DIR=/run/hmem/static \
    HMEM_MIGRATIONS_DIR=/opt/hmem/migrations
RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends ca-certificates curl grep jq; \
    rm -rf /var/lib/apt/lists/*; \
    printf 'hmem:x:10001:10001::/var/lib/hmem:/usr/sbin/nologin\n' >> /etc/passwd; \
    printf 'hmem:x:10001:\n' >> /etc/group; \
    mkdir -p /opt/hmem/static /opt/hmem/migrations /opt/hmem/provenance /opt/hmem/licenses /var/lib/hmem/.hmem/logs; \
    chown -R 10001:10001 /var/lib/hmem; \
    chmod 700 /var/lib/hmem /var/lib/hmem/.hmem /var/lib/hmem/.hmem/logs
COPY --from=pg-runtime-libs /opt/hmem/pg-libs/ /usr/lib/x86_64-linux-gnu/
COPY --from=pg-runtime-libs /opt/hmem/pg-licenses/ /opt/hmem/provenance/pg-libraries/
COPY --from=haskell-builder /opt/hmem/bin/hmem-server /usr/local/bin/hmem-server
COPY --from=haskell-builder /opt/hmem/bin/hmem-ctl /usr/local/bin/hmem-ctl
COPY --from=haskell-builder /opt/hmem/bin/hmem-mcp /usr/local/bin/hmem-mcp
COPY --from=haskell-builder /opt/hmem/bin/hmem-embedding-http-helper /usr/local/bin/hmem-embedding-http-helper
COPY --from=frontend-builder /workspace/hmem-server/static /opt/hmem/static
COPY hmem-server/migrations /opt/hmem/migrations
COPY --chmod=0755 docker/entrypoint.sh /usr/local/bin/hmem-entrypoint
COPY --chmod=0755 docker/healthcheck.sh /usr/local/bin/hmem-healthcheck
COPY LICENSE /opt/hmem/licenses/hmem-LICENSE
COPY --from=managed-bundle /manifest/managed-embedding-provenance.yaml /opt/hmem/managed-embedding/manifest/managed-embedding-provenance.yaml
COPY --from=managed-bundle /model/ /opt/hmem/managed-embedding/model/
COPY --from=managed-bundle /tei-runtime/ /opt/hmem/managed-embedding/tei-runtime/
COPY licenses/managed-embedding/ /opt/hmem/licenses/managed-embedding/
RUN set -eux; \
    chmod 0755 /opt/hmem/managed-embedding/tei-runtime/entrypoint.sh /opt/hmem/managed-embedding/tei-runtime/text-embeddings-router; \
    chmod -R a-w /opt/hmem/managed-embedding /opt/hmem/licenses; \
    for bin in /usr/local/bin/hmem-server /usr/local/bin/hmem-ctl /usr/local/bin/hmem-mcp /usr/local/bin/hmem-embedding-http-helper; do \
      ldd "$bin"; \
      if ldd "$bin" | grep 'not found'; then exit 1; fi; \
      sha256sum "$bin"; \
    done > /opt/hmem/provenance/executable-and-abi.txt; \
    dpkg-query -W -f='${Package} ${Version}\n' > /opt/hmem/provenance/image-packages.txt; \
    find /opt/hmem/managed-embedding /opt/hmem/licenses/managed-embedding -type f -print0 | sort -z | xargs -0 sha256sum > /opt/hmem/provenance/managed-assets.sha256; \
    test -f /opt/hmem/static/index.html; \
    set -- /opt/hmem/migrations/V*.sql; test -f "$1"
EXPOSE 8420
USER 10001:10001
STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/local/bin/hmem-entrypoint"]
CMD ["hmem-server"]
HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 CMD ["/usr/local/bin/hmem-healthcheck"]

# Keep this final stage independent of managed-bundle and GPU layers.
FROM runtime-base AS runtime
EXPOSE 8420
USER hmem:hmem
STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/local/bin/hmem-entrypoint"]
CMD ["hmem-server"]
HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 CMD ["/usr/local/bin/hmem-healthcheck"]
