# ===============================================================================================

FROM elixir:1.19-slim AS build

# Update system deps and install Hex, Rebar
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends build-essential ca-certificates && \
    apt-get clean && rm -rf /var/lib/apt/lists/* && \
    mix local.rebar --force && \
    mix local.hex --force

# Application folder
WORKDIR /usr/src/app

# Copy source code
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV MIX_ENV="prod"
ENV ERL_COMPILER_OPTIONS="[native, {hipe, [verbose, o3]}]"

# Download dependencies and build release
RUN mix deps.get --only prod && \
    mix compile && \
    mix release --path release

# ===============================================================================================

FROM debian:trixie-slim AS app

# Update system deps and add utils for healthcheck
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends openssl curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Application folder
RUN mkdir /app
WORKDIR /app

# Copy release from build container
COPY --from=build /usr/src/app/release ./

# User
RUN chown -R nobody: /app
USER nobody
ENV HOME=/app

# Check status
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

# Start release
ENTRYPOINT {{command}}
