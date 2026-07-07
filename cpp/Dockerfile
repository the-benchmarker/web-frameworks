# Production-grade C++ Dockerfile
# Optimized for security, performance, and minimal image size

FROM debian:trixie-slim AS build

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    build-essential \
    git \
    cmake \
    ca-certificates \
    {{#build_deps}}{{{.}}} {{/build_deps}} && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

ENV GIT_SSL_NO_VERIFY=1

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
  RUN {{{.}}}
{{/build}}

# Runtime stage
FROM debian:trixie-slim

WORKDIR /opt/web

# Install minimal runtime dependencies
RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    curl \
    ca-certificates \
    {{#bin_deps}}{{.}} {{/bin_deps}} && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /opt/web -s /bin/false appuser && \
    chown -R appuser:appuser /opt/web

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT {{{command}}}
