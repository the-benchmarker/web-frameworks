# Stage 1: Build
FROM rust:1.80-alpine as builder
RUN apk add --no-libc-dev build-base
WORKDIR /usr/src/app
COPY . .
RUN cargo build --release

# Stage 2: Runtime
FROM alpine:latest
WORKDIR /usr/local/bin
COPY --from=builder /usr/src/app/target/release/zeno-rs-actix .
COPY --from=builder /usr/src/app/app.zl .
EXPOSE 3000
CMD ["./zeno-rs-actix"]
