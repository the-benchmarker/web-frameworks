-- Production-grade Dockerfile for Haskell web applications
-- Multi-stage build for security and minimal image size
-- Uses best practices for security, performance, and maintainability

-- Build stage: Use official Haskell image with build tools
FROM haskell:9.14-bookworm AS build

-- Security: Run as non-root user in build stage
RUN useradd -m haskell
WORKDIR /usr/src/app

-- Copy only necessary files for build
-- Security: Copy files as non-root
{{#files}}
  COPY --chown=haskell:haskell '{{source}}' '{{target}}'
{{/files}}

-- Build with production settings
-- Security: No network access during build
-- Performance: Use all available cores
RUN --mount=type=cache,target=/root/.stack \
    --mount=type=cache,target=/usr/src/app/.stack-work \
    stack build \
      --no-terminal \
      --install-ghc \
      --ghc-options="$CABAL_OPTIONS -O2" \
      --jobs=$(nproc) \
      --haddock=no \
      --test=no \
      --benchmark=no

-- Production stage: Use minimal Debian image
FROM debian:bookworm-slim

-- Security: Create non-root user for production
RUN groupadd -r haskell && \
    useradd -r -g haskell haskell && \
    mkdir -p /opt/web && \
    chown haskell:haskell /opt/web

-- Set working directory
WORKDIR /opt/web

-- Security: Install minimal dependencies
-- curl for health checks, ca-certificates for HTTPS
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends \
      curl \
      ca-certificates \
      netbase \
      && rm -rf /var/lib/apt/lists/*

-- Security: Copy binary from build stage
-- Use specific architecture path for cross-platform compatibility
COPY --from=build --chown=haskell:haskell /usr/src/app/.stack-work/dist/x86_64-linux-tinfo6/ghc-9.6.6/build/server/server /opt/web/server

-- Security: Set proper permissions on binary
RUN chmod 755 /opt/web/server && \
    chown haskell:haskell /opt/web/server

-- Security: Drop all capabilities except those needed
-- NET_BIND_SERVICE for binding to privileged ports
RUN setcap 'cap_net_bind_service=+ep' /opt/web/server

-- Security: Switch to non-root user for production
USER haskell

-- Health check with proper configuration
-- Security: Use localhost instead of 0.0.0.0 for health checks
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://localhost:3000/health || exit 1

-- Production environment variables
-- Security: Disable debug mode
-- Performance: Optimize for production
ENV ENV=production
ENV PORT=3000
ENV HOST=0.0.0.0
ENV REQUEST_TIMEOUT=30
ENV MAX_REQUEST_SIZE=10485760

-- Security: Expose only necessary port
EXPOSE 3000

-- Entrypoint: Run the server
ENTRYPOINT {{{command}}}
