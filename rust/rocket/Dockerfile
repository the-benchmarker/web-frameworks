FROM rust

RUN /usr/local/cargo/bin/rustup install nightly
RUN /usr/local/cargo/bin/rustup default nightly

WORKDIR /usr/src/app
COPY . .

RUN cargo install
RUN cargo build --release

EXPOSE 3000
CMD ROKET_ENV=production target/release/server_rust_rocket
