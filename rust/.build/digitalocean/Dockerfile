FROM rust:1.43

WORKDIR /opt/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN cargo build --release
