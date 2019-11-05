FROM ruby:2.6-alpine

WORKDIR /usr/src/app

COPY . ./

RUN apk add build-base

RUN bundle install --without development test

{{#environment}}
ENV {{{.}}}
{{/environment}}

{{#command}}
  CMD {{{command}}}
{{/command}}

{{^command}}
  CMD bundle exec puma -p 3000 -e production -w $(nproc)
{{/command}}
