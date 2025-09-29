FROM clojure:temurin-25-tools-deps-trixie-slim

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

FROM eclipse-temurin:25-jre

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} /opt/bin/{{{.}}} 
{{/binaries}}

ENTRYPOINT {{command}}

RUN apt-get -qq update
RUN apt-get -qy install curl

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
