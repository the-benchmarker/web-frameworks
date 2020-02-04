FROM elixir:1.10-alpine AS build
RUN apk add --update git build-base
RUN mix local.rebar --force
RUN mix local.hex --force
WORKDIR /usr/src/app
COPY . ./
ENV MIX_ENV prod
RUN mix deps.get --only prod
RUN mix release --path release

FROM alpine:latest AS app
RUN apk add --update bash openssl
RUN mkdir /app
WORKDIR /app
COPY --from=build /usr/src/app/release ./
RUN chown -R nobody: /app
USER nobody
ENV HOME /app
CMD bin/default start
