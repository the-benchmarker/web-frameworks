FROM debian:stable AS build

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential ldc dub openssl libssl-dev zlib1g-dev llvm-dev

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release-nobounds --compiler=ldc2

FROM debian:stable

WORKDIR /opt/web

RUN echo "* hard nofile 16768" >> /etc/limits.conf
RUN apt-get -qq update && apt-get -qy install openssl ldc

COPY --from=build /usr/src/app/server /opt/web/server

USER nobody

CMD {{{command}}}
