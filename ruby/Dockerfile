FROM ruby:3.0.0-alpine

WORKDIR /usr/src/app

COPY . ./

RUN apk add build-base

RUN bundle config set without 'development test'
RUN bundle install

{{#environment}}
ENV {{{.}}}
{{/environment}}

{{#command}}
  CMD {{{command}}}
{{/command}}

{{^command}}
  CMD bundle exec puma -p 3000 -e production -w $(nproc)
{{/command}}
