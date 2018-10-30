FROM rust:1.30

WORKDIR /usr/src/app

COPY Cargo.toml ./
COPY src src

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 3000

CMD target/release/server
