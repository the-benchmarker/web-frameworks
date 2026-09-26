
{{#language.compiler.gradle}}
  FROM gradle:9-jdk{{{language.version}}} AS build
{{/language.compiler.gradle}}

{{#language.compiler.maven}}
  {{#language.version}}
    FROM maven:3-eclipse-temurin-{{{.}}} AS build
  {{/language.version}}
  {{^language.version}}
    FROM maven:3-eclipse-temurin-25 AS build
  {{/language.version}}
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



{{#graalvm}}
  FROM ghcr.io/graalvm/jdk-community:25
  RUN microdnf install -y curl tar && microdnf clean all
{{/graalvm}}

{{^graalvm}}

{{#language.version}}
  FROM eclipse-temurin:{{{.}}}-jre
{{/language.version}}

{{^language.version}}
  FROM eclipse-temurin:25.0.4_7-jre
{{/language.version}}

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

{{/graalvm}}

WORKDIR /opt/web

{{#binaries}}
  COPY --from=build /usr/src/web/{{{.}}} {{{.}}}
{{/binaries}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

RUN ln -sfv "$JAVA_HOME/bin/java" /usr/bin/java

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
