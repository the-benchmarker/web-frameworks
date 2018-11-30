FROM rust:1.30.1

WORKDIR /usr/src/app

COPY Cargo.toml ./
COPY src src

# install musl
RUN apt-get -qq update
RUN apt-get install -qy musl-tools
RUN rustup target add x86_64-unknown-linux-musl

# build static binary
RUN RUSTFLAGS="-C target-cpu=native"  cargo build --release --target x86_64-unknown-linux-musl

RUN cp target/x86_64-unknown-linux-musl/release/server .