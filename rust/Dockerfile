FROM rust:1.49 AS build

WORKDIR /usr/src/app

COPY . ./

RUN cargo install --path .

CMD /usr/local/cargo/bin/server
