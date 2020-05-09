FROM google/dart:2.10

WORKDIR /opt/web

{{#files}}
    COPY {{{.}}} {{{.}}}
{{/files}}

RUN pub get

CMD dart main.dart
