FROM crystallang/crystal:1.20 AS build

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update && \
    apt-get -y install --no-install-recommends \
      ca-certificates \
      {{#build_deps}}{{{.}}} {{/build_deps}} && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get clean
{{/build_deps.length}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN shards lock

RUN --mount=type=cache,id=shards-lib-{{language.version}},target=lib \
  --mount=type=cache,id=shards-cache-{{language.version}},target=/root/.cache/shards \
  shards install --production

RUN --mount=type=cache,id=shards-lib-{{language.version}},target=lib \
  shards build --release --no-debug

# Runtime stage
FROM debian:trixie-slim

WORKDIR /app

# Install minimal runtime dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    {{#runtime_deps}}{{{.}}} {{/runtime_deps}} && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

COPY --from=build /usr/src/app/bin/server /app/bin/server

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /app -s /bin/false appuser && \
    chown -R appuser:appuser /app

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT {{{command}}}
