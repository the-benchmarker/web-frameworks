FROM curlimages/curl AS curl

{{#image}}
  FROM {{{.}}}
{{/image}}
{{^image}}
  FROM alpine:latest
{{/image}}

WORKDIR /usr/src/app

{{#deps}}
  RUN apk add {{{.}}}
{{/deps}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

COPY --from=curl /usr/bin/curl curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}
