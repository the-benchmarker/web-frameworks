FROM elixir:1.13-slim AS build

# Update system deps
RUN apt-get -qq update
RUN apt-get -qy install build-essential

# Instal Hex, Rebar and Rebar3
RUN mix local.rebar --force
RUN mix local.hex --force

# Application folder
WORKDIR /usr/src/app

# Copy source code
COPY . ./

ENV MIX_ENV="prod"
ENV ERL_COMPILER_OPTIONS="[native, {hipe, [verbose, o3]}]"

# Download dependencies
RUN mix deps.get --only prod

# Build release
RUN mix compile
RUN mix release --path release

# ===============================================================================================

FROM debian AS app

# Update system deps
RUN apt-get -qq update && apt-get -qy install openssl

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
CMD bin/server start
