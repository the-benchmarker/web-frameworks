FROM dlang2/ldc-ubuntu AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN dub build -b release --compiler=ldc2 --verbose

FROM ubuntu

RUN apt-get -qq update
{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

COPY --from=build /usr/src/app/server /usr/src/app/server

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
