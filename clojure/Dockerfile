{{#language.compiler.lein}}
  {{#language.java.version}}
    FROM clojure:temurin-{{{.}}}-lein-trixie-slim AS build
  {{/language.java.version}}
  {{^language.java.version}}
    FROM clojure:temurin-25-lein-trixie-slim AS build
  {{/language.java.version}}
{{/language.compiler.lein}}
{{^language.compiler.lein}}
  {{#language.java.version}}
    FROM clojure:temurin-{{{.}}}-tools-deps-trixie-slim AS build
  {{/language.java.version}}
  {{^language.java.version}}
    FROM clojure:temurin-25-tools-deps-trixie-slim AS build
  {{/language.java.version}}
{{/language.compiler.lein}}

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

FROM eclipse-temurin:25.0.1_8-jre

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} /opt/bin/{{{.}}} 
{{/binaries}}

ENTRYPOINT {{command}}

RUN apt-get -qq update
RUN apt-get -qy install curl

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
