FROM rust:1.47 AS build

WORKDIR /usr/src/app

RUN apt-get update && apt-get -y install musl-tools --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

RUN apt-get -y install musl-tools --no-install-recommends

RUN rustup target add x86_64-unknown-linux-musl

COPY . ./

RUN cargo build --target x86_64-unknown-linux-musl --release

# hadolint ignore=DL3006
FROM alpine

COPY --from=build /usr/src/app/target/x86_64-unknown-linux-musl/release/server /usr/src/app/target/x86_64-unknown-linux-musl/release/server

CMD /usr/src/app/target/x86_64-unknown-linux-musl/release/server
