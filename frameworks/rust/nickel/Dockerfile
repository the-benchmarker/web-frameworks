FROM rust

WORKDIR /usr/src/app

COPY Cargo.toml ./
COPY src src

RUN cargo install
RUN cargo build --release

EXPOSE 3000
CMD target/release/server
