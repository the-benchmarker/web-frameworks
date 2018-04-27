FROM rustlang/rust:nightly

WORKDIR /usr/src/app

RUN rustup default nightly
RUN rustup show

COPY Cargo.toml ./
COPY src src

RUN cargo install
RUN cargo build --release

ENV ROCKET_ENV=production

EXPOSE 3000

CMD target/release/server
