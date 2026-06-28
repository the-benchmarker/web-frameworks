# Production-grade Dockerfile for D language frameworks
# Multi-stage build for optimized production deployment

# Build stage
FROM debian:trixie-slim AS build

# Install build dependencies
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    zlib1g-dev \
    libssl-dev \
    clang \
    libxml2 \
    xz-utils \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/*

# Install LDC compiler
RUN curl -L -o ldc2.tar.xz https://github.com/ldc-developers/ldc/releases/download/v1.42.0/ldc2-1.42.0-linux-{{#arch}}{{{.}}}{{/arch}}.tar.xz && \
  tar xf ldc2.tar.xz && \
  rm ldc2.tar.xz && \
  mv ldc2* ldc2

# Configure environment
ENV PATH="$PATH:$CWD/ldc2/bin" \
  LDC=ldc2 \
  DEBIAN_FRONTEND=noninteractive \
  TZ=UTC

# Set up workspace
WORKDIR /usr/src/app

# Copy source files
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

# Build for production with optimizations
RUN dub build -b production --compiler=ldc2 --config=production

# Runtime stage
FROM debian:trixie-slim

# Install minimal runtime dependencies
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    libc6 \
    libssl3 \
    zlib1g && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean && \
  rm -rf /tmp/* /var/tmp/*

# Create non-root user for security
RUN useradd -r -u 1000 -g 1000 -m -d /opt/web -s /bin/false appuser

# Set up workspace
WORKDIR /opt/web

# Copy built binary from build stage
COPY --from=build --chown=appuser:appuser /usr/src/app/server /opt/web/server

# Security: Drop all privileges
USER appuser

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

# Run as non-root user
ENTRYPOINT ["/opt/web/server"]
