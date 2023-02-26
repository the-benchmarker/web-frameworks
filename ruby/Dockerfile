FROM ruby:3.2-alpine

WORKDIR /usr/src/app

{{#sources}}
  COPY '{{source}}' '{{target}}'
{{/sources}}

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