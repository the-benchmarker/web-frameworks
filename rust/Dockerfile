FROM rust:1.65-slim AS build

WORKDIR /usr/src/app

RUN apt-get -qy update

{{#build_deps}}
  RUN apt-get -qy install {{{.}}}
{{/build_deps}}

COPY . ./

RUN cargo build --release && strip target/release/server

FROM debian:buster-slim

COPY --from=build /usr/src/app/target/release/server /usr/src/app/target/release/server

CMD /usr/src/app/target/release/server
