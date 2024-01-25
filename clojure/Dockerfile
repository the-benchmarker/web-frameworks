{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM clojure:temurin-21-tools-deps-jammy
{{/image}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN clojure -Auberjar
{{/build}}

FROM eclipse-temurin:21.0.2_13-jre

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} /opt/bin/{{{.}}} 
{{/binaries}}

{{#command}}
  ENTRYPOINT {{{.}}}
{{/command}}
