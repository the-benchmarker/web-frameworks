FROM rust:1.84-slim AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN cargo build --release && strip target/release/server

FROM debian:stable-slim

COPY --from=build /usr/src/app/target/release/server /usr/src/app/target/release/server

RUN apt-get -qq update
RUN apt-get -qy install curl

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
