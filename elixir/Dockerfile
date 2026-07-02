#
# Build stage
#
FROM elixir:1.20-slim AS build

# Update system deps
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

# Instal Hex, Rebar and Rebar3
RUN mix local.rebar --force && \
  mix local.hex --force

# Application folder
WORKDIR /usr/src/app

# Copy source code
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV MIX_ENV="prod"

# Download dependencies
RUN mix deps.get --only prod

# Compile deps
RUN mix deps.compile

# Build release
RUN mix compile
RUN mix release --path release



#
# Runtime stage
#
FROM debian:trixie-slim

# Application folder
WORKDIR /app

# Update system deps
# Install minimal runtime dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    openssl \
    curl \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

# Set environment
ENV LANG=C.UTF-8 \
  LANGUAGE=C.UTF-8 \
  MIX_ENV="prod"

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /app -s /bin/false appuser

# Copy release from build container
COPY --from=build --chown=appuser:appuser /usr/src/app/release ./

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

# Start release
ENTRYPOINT {{{command}}}
