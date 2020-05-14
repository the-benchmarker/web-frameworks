FROM elixir:1.9-alpine AS build

# Update system deps
RUN apk add --update git build-base

# Instal Hex, Rebar and Rebar3
RUN mix local.rebar --force
RUN mix local.hex --force

# Application folder
WORKDIR /usr/src/app

# Copy source code
{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

ENV MIX_ENV="prod"
ENV ERL_COMPILER_OPTIONS="[native, {hipe, [verbose, o3]}]"

# Download dependencies
RUN mix deps.get --only prod

# Build release
RUN mix compile
RUN mix release --path release

# ===============================================================================================

FROM alpine:latest AS app

# Update system deps
RUN apk add --update bash openssl

# Application folder
RUN mkdir /app
WORKDIR /app

# Copy release from build container
COPY --from=build /usr/src/app/release ./

# User
RUN chown -R nobody: /app
USER nobody
ENV HOME /app

# Start release
CMD {{#command}}{{{.}}}{{/command}}
