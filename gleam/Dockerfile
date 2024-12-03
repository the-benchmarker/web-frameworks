{{#language.image}}
  FROM gleamlang/gleam
{{/language.image}}
{{^language.image}}
  FROM 
{{/language.image}}

WORKDIR /usr/src/app

{{#files}}
COPY '{{source}}' '{{target}}'
{{/files}}

RUN gleam build

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
