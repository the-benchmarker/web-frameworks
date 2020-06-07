FROM ruby:2.7-alpine

WORKDIR /opt/web

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

RUN apk add build-base --no-cache

RUN ln -sfv /usr/local/bin/ruby /usr/bin/ruby
RUN ln -sfv /usr/local/bin/bundle /usr/bin/bundle

{{#alpine-deps}}
  RUN apk add {{{.}}} {{{.}}}
{{/alpine-deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

CMD {{#command}}{{{.}}}{{/command}}
