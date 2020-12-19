FROM rust:1.48 AS build

WORKDIR /usr/src/app

RUN apt-get update && apt-get -y install musl-tools --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

RUN apt-get -y install musl-tools --no-install-recommends

RUN rustup target add x86_64-unknown-linux-musl

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN cargo build --target x86_64-unknown-linux-musl --release
RUN cp target/x86_64-unknown-linux-musl/release/server target/release/server

# hadolint ignore=DL3006
FROM alpine

WORKDIR /opt/web

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{command}}