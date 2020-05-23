FROM google/dart:2.8

WORKDIR /opt/web

{{#files}}
    COPY {{{.}}} {{{.}}}
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

CMD {{#command}}{{{.}}}{{/command}}
