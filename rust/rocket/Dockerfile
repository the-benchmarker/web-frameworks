FROM rust

WORKDIR /usr/src/app

RUN rustup default nightly-2018-04-28
RUN rustup update

COPY Cargo.toml ./
COPY src src

ENV ROCKET_ENV=production

RUN cargo build --release

EXPOSE 3000

CMD target/release/server
