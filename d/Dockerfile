FROM debian:bookworm AS build

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential wget openssl ldc dub libssl-dev zlib1g-dev

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2 --verbose

FROM debian:bookworm

WORKDIR /usr/src/app

RUN apt-get -qq update && apt-get -qy install openssl libssl-dev

COPY --from=build /usr/src/app/server /usr/src/app/server

{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
