FROM ruby:2.6

WORKDIR /usr/src/app

COPY . ./

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
