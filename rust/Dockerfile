# Production-grade Rust Dockerfile
# Optimized for security, performance, and minimal image size

FROM rust:1.96-slim AS build

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* && \
    apt-get clean

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

# Build with production optimizations
RUN cargo build --release \
  --config 'profile.release.lto=true' \
  --config 'profile.release.panic="abort"' \
  --config 'profile.release.codegen-units=1' \
  --config 'profile.release.strip=true' \
  --config 'profile.release.opt-level=3'

# Runtime stage
FROM debian:stable-slim

# Install minimal runtime dependencies
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* && \
    apt-get clean

COPY --from=build /usr/src/app/target/release/server /usr/src/app/target/release/server

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT {{{command}}}
