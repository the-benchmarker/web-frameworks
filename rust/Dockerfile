FROM rust:1.68-slim AS build

WORKDIR /usr/src/app

COPY . ./

RUN cargo build --release && strip target/release/server

FROM debian:stable-slim

COPY --from=build /usr/src/app/target/release/server /usr/src/app/target/release/server

CMD {{command}}
