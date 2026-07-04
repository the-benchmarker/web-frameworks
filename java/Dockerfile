
{{#language.compiler.gradle}}
  FROM gradle:9-jdk25 AS build
{{/language.compiler.gradle}}
{{#language.compiler.maven}}
  {{#language.java}}
    FROM maven:3-eclipse-temurin-{{{.}}} AS build
  {{/language.java}}
  {{^language.java}}
    FROM maven:3-eclipse-temurin-25 AS build
  {{/language.java}}
{{/language.compiler.maven}}

WORKDIR /usr/src/web

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update && \
    apt-get -y install {{#build_deps}}{{{.}}} {{/build_deps}} && \
    rm -rf /var/lib/apt/lists/*
{{/build_deps.length}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#patch}}
  {{{.}}}
{{/patch}}

{{#build_fix}}
  RUN {{{.}}}
{{/build_fix}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN mvn package
{{/build}}



{{#language.java}}
  FROM eclipse-temurin:{{{.}}}-jre
{{/language.java}}
{{^language.java}}
  FROM eclipse-temurin:25.0.3_9-jre
{{/language.java}}

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /opt/web

{{#binaries}}
  COPY --from=build /usr/src/web/{{{.}}} {{{.}}}
{{/binaries}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

RUN ln -sfv /opt/java/openjdk/bin/java /usr/bin/java

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
