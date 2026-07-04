FROM rust:1.96-slim AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN cargo build --release  \
  --config  'profile.release.lto=true' \
  --config  'profile.release.panic="abort"' \
  --config  'profile.release.codegen-units=1'



FROM debian:stable-slim

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/target/release/server /usr/src/app/target/release/server

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
