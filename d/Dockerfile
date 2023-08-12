FROM debian:bookworm AS build

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential openssl ldc dub libssl-dev zlib1g-dev

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2 --verbose

FROM debian:bookworm

WORKDIR /opt/web

RUN apt-get -qq update && apt-get -qy install openssl ldc

COPY --from=build /usr/src/app/server /opt/web/server

CMD {{{command}}}
