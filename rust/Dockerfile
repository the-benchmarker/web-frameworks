FROM rust:1.44 AS build

WORKDIR /usr/src/app

RUN apt-get -qq update && apt-get -qy install musl-tools

RUN rustup target add x86_64-unknown-linux-musl

COPY . ./

RUN cargo build --target x86_64-unknown-linux-musl --release

FROM alpine:latest

COPY --from=build /usr/src/app/target/x86_64-unknown-linux-musl/release/server /usr/src/app/target/x86_64-unknown-linux-musl/release/server

CMD /usr/src/app/target/x86_64-unknown-linux-musl/release/server
