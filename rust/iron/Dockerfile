# Static builder for Rust (https://github.com/emk/rust-musl-builder)
FROM ekidd/rust-musl-builder AS builder
COPY . /home/rust/src
RUN cargo build --release

# New image from scratch (minimal sized)
FROM scratch
COPY --from=builder /home/rust/src/target/x86_64-unknown-linux-musl/release/server_rust_iron /server_rust_iron
EXPOSE 3000
ENTRYPOINT ["/server_rust_iron"]