{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM clojure:tools-deps-bookworm-slim
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

FROM eclipse-temurin:21.0.5_11-jre

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} /opt/bin/{{{.}}} 
{{/binaries}}

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK --interval=1m --timeout=600s --retries=3 CMD curl --fail http://0.0.0.0:3000 || exit 1

{{#command}}
  ENTRYPOINT {{{.}}}
{{/command}}
