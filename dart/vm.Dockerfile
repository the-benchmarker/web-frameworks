FROM dart:3.8

WORKDIR /app
COPY pubspec.yaml pubspec.yaml
RUN dart pub get --no-precompile

{{#files}}
COPY '{{source}}' '{{target}}'
{{/files}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
