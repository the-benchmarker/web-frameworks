#
# Build stage
#
FROM elixir:1.20-slim AS build

# Update system deps
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/*

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
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends openssl curl && \
  rm -rf /var/lib/apt/lists/*

# Set environment
ENV LANG=C.UTF-8 \
  LANGUAGE=C.UTF-8 \
  MIX_ENV="prod"

# User
RUN chown nobody /app

# Copy release from build container
COPY --from=build --chown=nobody:root /usr/src/app/release ./

USER nobody

# Check status
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

# Start release
ENTRYPOINT {{{command}}}
