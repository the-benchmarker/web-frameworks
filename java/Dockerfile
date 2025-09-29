
{{#language.compiler.gradle}}
  FROM gradle:9-jdk25 AS build
{{/language.compiler.gradle}}
{{#language.compiler.maven}}
  FROM maven:3-eclipse-temurin-25 AS build
{{/language.compiler.maven}}

WORKDIR /usr/src/web

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}


{{#patch}}
  {{{.}}}
{{/patch}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN mvn package
{{/build}}

FROM eclipse-temurin:25-jre


WORKDIR /opt/web


{{#binaries}}
  COPY --from=build /usr/src/web/{{{.}}} {{{.}}}
{{/binaries}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

RUN  ln -sfv /opt/java/openjdk/bin/java /usr/bin/java

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
