FROM rust

WORKDIR /usr/src/app
COPY . .

RUN cargo install
RUN cargo build --release

EXPOSE 3000
CMD target/release/server_rust_iron
