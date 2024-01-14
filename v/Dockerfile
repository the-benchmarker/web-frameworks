FROM thevlang/vlang:alpine-dev as build

WORKDIR /app
RUN v up

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN v . -prod -o server

FROM alpine:latest

WORKDIR /app

COPY --from=build /app /app

CMD {{command}}
