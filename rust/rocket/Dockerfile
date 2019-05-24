FROM rust:1.35.0

WORKDIR /usr/src/app

RUN rustup default nightly

COPY Cargo.toml ./
COPY src src

ENV ROCKET_ENV=production

RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

EXPOSE 3000

CMD target/release/server
