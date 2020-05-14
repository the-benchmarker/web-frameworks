FROM rust:1.44 AS build

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

# hadolint ignore=DL3006
FROM alpine

WORKDIR /opt/web

COPY --from=build /usr/src/app/target/x86_64-unknown-linux-musl/release/server /usr/src/app/target/x86_64-unknown-linux-musl/release/server

CMD /opt/web/target/x86_64-unknown-linux-musl/release/server
