FROM alpine:3.18
RUN apk add --no-cache ldc dub ldc-runtime

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2 --verbose

FROM ubuntu:bionic

RUN apt-get -qq update
{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

COPY --from=build /usr/src/app/server /usr/src/app/server

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
