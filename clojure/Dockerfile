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



FROM eclipse-temurin:25.0.3_9-jre

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} ./{{{.}}}
{{/binaries}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
