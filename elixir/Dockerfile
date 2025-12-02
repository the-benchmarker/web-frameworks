# ===============================================================================================

FROM elixir:1.19-slim AS build

# Update system deps
RUN apt-get -qq update
RUN apt-get -qy install --no-install-recommends build-essential ca-certificates \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# Instal Hex, Rebar and Rebar3
RUN mix local.rebar --force
RUN mix local.hex --force

# Application folder
WORKDIR /usr/src/app

# Copy source code
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV MIX_ENV="prod"
ENV ERL_COMPILER_OPTIONS="[native, {hipe, [verbose, o3]}]"

# Download dependencies
RUN mix deps.get --only prod

# Build release
RUN mix compile
RUN mix release --path release

# ===============================================================================================

FROM debian:bookworm-slim AS app

# Update system deps
RUN apt-get -qq update && apt-get -qy install --no-install-recommends openssl

# Application folder
RUN mkdir /app
WORKDIR /app

# Copy release from build container
COPY --from=build /usr/src/app/release ./

# Add utils for healtcheck
RUN apt-get -qq update
RUN apt-get -qy install curl

# User
RUN chown -R nobody: /app
USER nobody
ENV HOME=/app

# Check status
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

# Start release
ENTRYPOINT {{command}}
