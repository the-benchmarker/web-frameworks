FROM debian:sid AS build

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -y install build-essential wget openssl ldc

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2 --verbose

{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
