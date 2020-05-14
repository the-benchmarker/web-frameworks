FROM rust:1.43

WORKDIR /usr/src/app

RUN apt-get -qq update && apt-get -qy install musl-tools

RUN rustup target add x86_64-unknown-linux-musl

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN cargo build --target x86_64-unknown-linux-musl --release

FROM alpine

WORKDIR /opt/web

COPY --from=0 /usr/src/app/target/x86_64-unknown-linux-musl/release/server target/x86_64-unknown-linux-musl/release/server

CMD target/x86_64-unknown-linux-musl/release/server
