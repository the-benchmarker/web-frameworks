# Production-grade Objective-C Dockerfile
# Optimized for security, performance, and minimal image size

# Build stage
FROM debian:trixie-slim AS build

WORKDIR /usr/src/app

# Install build dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    clang \
    libdispatch-dev \
    libblocksruntime-dev \
    libobj4-dev \
    build-essential \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#build}}
  RUN {{{.}}}
{{/build}}
{{^build}}
  RUN clang -O2 -o server {{#files}}{{source}} {{/files}}
{{/build}}

# Runtime stage
FROM debian:trixie-slim

WORKDIR /usr/src/app

# Install minimal runtime dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    libobjc4 && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}
{{^binaries}}
  COPY --from=build /usr/src/app/server /usr/src/app/server
{{/binaries}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}} 
