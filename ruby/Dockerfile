FROM ruby:3.2-slim

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN apk add build-base {{#deps}}{{{.}}}{{/deps}}



{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN bundle config set without 'development test'
RUN bundle install
{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

CMD {{{command}}}