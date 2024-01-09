FROM nimlang/nim:2.0.2

RUN apt-get -qq update

{{#deps}}
  RUN apt-get -qy install {{{.}}}
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

FROM debian:stable

WORKDIR /usr/src/app

RUN apt-get -qq update

RUN apt-get -qy install \
    libssl-dev \
    libpcre3-dev

{{#custom_files}}
  COPY --from=0 /usr/src/app/{{{.}}} /usr/src/app/{{{.}}}
{{/custom_files}}
{{^custom_files}}
  COPY --from=0 /usr/src/app/server /usr/src/app/server
{{/custom_files}}

{{#command}}
  CMD {{{.}}}
{{/command}}
{{^command}}
  CMD /usr/src/app/server
{{/command}}
