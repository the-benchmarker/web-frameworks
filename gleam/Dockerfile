FROM ghcr.io/gleam-lang/gleam:v1.11.1-erlang

WORKDIR /opt/web

{{#files}}
COPY '{{source}}' '{{target}}'
{{/files}}

RUN gleam build
RUN gleam export erlang-shipment

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
