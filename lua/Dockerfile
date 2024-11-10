{{#image}}
  FROM {{{.}}}
{{/image}}
{{^image}}
  FROM alpine:latest
{{/image}}

WORKDIR /usr/src/app

{{#build}}
  RUN {{{.}}}
{{/build}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK --interval=1m --timeout=60s --retries=3 CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}
