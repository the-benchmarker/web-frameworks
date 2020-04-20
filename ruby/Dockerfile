{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
FROM ruby:2.7-alpine
{{/image}}

WORKDIR /usr/src/app

{{^image}}
RUN apk add build-base
{{/image}}

RUN bundle config set without 'development test'

COPY Gemfile .
RUN bundle install

COPY . ./

{{#environment}}
ENV {{{.}}}
{{/environment}}

{{#command}}
  CMD {{{command}}}
{{/command}}

{{^command}}
  CMD bundle exec puma -p 3000 -e production -w $(nproc)
{{/command}}
