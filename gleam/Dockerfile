# ===========================================================================
# Gleam Production Dockerfile
# Multi-stage build with production-grade optimizations
# ===========================================================================

# ===========================================================================
# Build Stage
# ===========================================================================

FROM ghcr.io/gleam-lang/gleam:v1.17.0-scratch AS gleam

FROM erlang:29-alpine AS build

# Configure build environment
ENV GLEAM_VERSION=1.17.0 \
    MIX_ENV=prod

WORKDIR /opt/web

# Copy Gleam binary from scratch image
COPY --from=gleam /bin/gleam /bin/gleam

# Copy application files
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

# Build and export for production
RUN gleam build --no-debug-info \
    && gleam export erlang-shipment

# ===========================================================================
# Runtime Stage
# ===========================================================================

FROM erlang:29-alpine

# Set runtime environment
ENV MIX_ENV=prod \
    ERLANG_COOKIE=production_cookie \
    HEART_BEAT_TIMEOUT=60

# Install curl for health checks
RUN apk add --no-cache curl

# Create non-root user for security
RUN addgroup -S appgroup && adduser -S appuser -G appgroup

WORKDIR /app

# Copy built application from build stage
COPY --from=build --chown=appuser:appgroup /opt/web/build/erlang-shipment .

# Switch to non-root user
USER appuser:appgroup

# Health check with improved configuration
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl --fail --silent --max-time 3 http://0.0.0.0:3000/ || exit 1

{{#environment}}
    ENV {{{.}}}
{{/environment}}

# ===========================================================================
# Entrypoint
# ===========================================================================

ENTRYPOINT {{{command}}}

CMD {{{options}}}

# ===========================================================================
# Security Best Practices Applied:
# - Multi-stage build to reduce image size
# - Non-root user for application runtime
# - Minimal base image (erlang:alpine)
# - No build tools in production image
# - Health checks configured with proper timeouts
# - Production environment variables set
# - Debug information disabled in build
# ===========================================================================
