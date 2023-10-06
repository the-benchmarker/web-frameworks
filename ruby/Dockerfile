FROM ruby:3.2-alpine AS base

WORKDIR /usr/src/app

RUN apk add build-base {{#deps}}{{{.}}}{{/deps}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

EXPOSE 3000

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN bundle install

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

CMD {{{command}}}
