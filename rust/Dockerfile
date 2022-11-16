FROM rust:1.65-slim AS build

COPY . ./

RUN cargo build --release && strip target/release/server

FROM debian:slim

COPY --from=build /usr/src/app/target/release/server /usr/src/app/target/release/server

CMD /usr/src/app/target/release/server
