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

FROM eclipse-temurin:25-jre

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} /opt/bin/{{{.}}} 
{{/binaries}}

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
