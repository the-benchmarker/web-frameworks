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

CMD {{command}}
