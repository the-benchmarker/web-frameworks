FROM akito13/nim:2.0.8-alpine AS build

RUN apk update

{{#deps}}
  RUN apk add {{{.}}}
{{/deps}}

## set working directory
WORKDIR /usr/src/app

## Nim environment
ENV NIM_ENV=production
ENV NIMBLE_DIR=/home/nim/.nimble
ENV PATH=$PATH:/home/nim/.nimble/bin


{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV PATH $PATH:/root/.nimble/bin

{{#build_with}}
  RUN {{{.}}}
{{/build_with}}
{{^build_with}}
  RUN nimble install -y
{{/build_with}}

{{#build_command}}
  RUN {{{.}}}
{{/build_command}}
{{^build_command}}
  RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --assertions:off \
    --warnings:off \
    --hints:off \
    -d:release \
    --opt:speed \
    --passC:-flto \
    --passL:-flto \
    server.nim
{{/build_command}}

FROM alpine:latest

WORKDIR /usr/src/app

RUN apk update

RUN apk add openssl-dev pcre-dev curl

{{#custom_files}}
  COPY --from=build /usr/src/app/{{{.}}} /usr/src/app/{{{.}}}
{{/custom_files}}

{{^custom_files}}
  COPY --from=build /usr/src/app/server /usr/src/app/server
{{/custom_files}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

{{#command}}
  ENTRYPOINT {{{.}}}
{{/command}}
{{^command}}
  ENTRYPOINT /usr/src/app/server
{{/command}}
