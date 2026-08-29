FROM ghcr.io/gleam-lang/gleam:v1.17.0-scratch AS gleam



FROM erlang:29-alpine AS build

WORKDIR /opt/web

COPY --from=gleam /bin/gleam /bin/gleam

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN gleam build

RUN gleam export erlang-shipment



FROM erlang:29-alpine

WORKDIR /app

COPY --from=build /opt/web/build/erlang-shipment .

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

CMD {{{options}}}
