
{{#language.compiler.gradle}}
  {{#graalvm}}
    FROM gradle:9-jdk{{language.graalvm_java.version}} AS build
  {{/graalvm}}
  {{^graalvm}}
    FROM gradle:9-jdk{{language.version}} AS build
  {{/graalvm}}
{{/language.compiler.gradle}}

{{#language.compiler.maven}}
  {{#graalvm}}
    FROM maven:3-eclipse-temurin-{{language.graalvm_java.version}} AS build
  {{/graalvm}}
  {{^graalvm}}
    FROM maven:3-eclipse-temurin-{{language.version}} AS build
  {{/graalvm}}
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
  FROM ghcr.io/graalvm/jdk-community:{{language.graalvm_java.version}}
  RUN microdnf install -y curl tar && microdnf clean all
{{/graalvm}}

{{^graalvm}}
  FROM eclipse-temurin:{{language.version}}-jre

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
