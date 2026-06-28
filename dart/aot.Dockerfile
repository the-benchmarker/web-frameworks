# Production-grade Dart AOT Dockerfile
# Multi-stage build for optimized production deployment with security best practices

# Curl stage for health checks
FROM curlimages/curl AS curl

# Build stage
FROM dart:3.12 AS build

# Set the working directory
WORKDIR /app

# Install ca-certificates for HTTPS support
RUN apt-get update && apt-get install -y ca-certificates && rm -rf /var/lib/apt/lists/*

# Configure environment for production
ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    DART_VM_OPTIONS=--no-enable-asserts

# Add pubspec.yaml and get dependencies
COPY pubspec.yaml pubspec.yaml
RUN dart pub get --no-precompile --verbose

# Copy app source code and refetch dependencies to cache
{{#files}}
    COPY '{{source}}' '{{target}}'
{{/files}}

RUN dart pub get --offline --no-precompile

# AOT compile `server.dart` to server executable with production optimizations
RUN dart compile exe server.dart -o server \
    --release \
    --no-source-maps \
    --target=platform

# Runtime stage
FROM alpine:3.20

# Install minimal runtime dependencies
RUN apk add --no-cache curl ca-certificates && \
    rm -rf /tmp/* /var/tmp/*

# Create non-root user for security
RUN adduser -D -u 1000 -g 1000 appuser

# Set up workspace
WORKDIR /app

# Copy built binary from build stage
COPY --from=build --chown=appuser:appuser /app/server /app/server

# Copy runtime from build stage
COPY --from=build /runtime/ /runtime/

# Copy curl for health checks
COPY --from=curl /usr/bin/curl /usr/bin/curl

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

# Run as non-root user
ENTRYPOINT ["/app/server"]
