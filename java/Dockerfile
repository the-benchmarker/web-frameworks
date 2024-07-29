
{{#language.compiler.gradle}}
  FROM gradle:jdk21-jammy AS build
{{/language.compiler.gradle}}
{{#language.compiler.maven}}
  {{#language.version}}
    FROM maven:3-eclipse-temurin-{{{.}}} AS build
  {{/language.version}}
  {{^language.version}}
    FROM maven:3-eclipse-temurin-21 AS build
  {{/language.version}}
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

{{#language.version}}
  FROM eclipse-temurin:{{{.}}}-jre
{{/language.version}}
{{^language.version}}
  FROM eclipse-temurin:21.0.4_7-jre
{{/language.version}}


WORKDIR /opt/web

{{#fix}}
  RUN {{{.}}}
{{/fix}}

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

CMD {{{command}}}
