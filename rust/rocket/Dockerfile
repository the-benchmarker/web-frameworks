FROM rust:1.30

WORKDIR /usr/src/app

RUN rustup default nightly

COPY Cargo.toml ./
COPY src src

ENV ROCKET_ENV=production

RUN cargo build --release

EXPOSE 3000

CMD target/release/server
