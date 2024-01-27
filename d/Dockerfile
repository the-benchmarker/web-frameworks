FROM debian:trixie AS build

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential ldc dub openssl libssl-dev zlib1g-dev llvm-dev

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2

FROM debian:trixie

WORKDIR /opt/web

RUN apt-get -qq update && apt-get -qy install openssl ldc

COPY --from=build /usr/src/app/server /opt/web/server

USER nobody

ENTRYPOINT {{{command}}}
