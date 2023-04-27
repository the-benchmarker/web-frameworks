{{#image}}
  FROM {{{.}}} AS build
{{/image}}

{{^image}}
  FROM maven:3-eclipse-temurin-17 AS build
{{/image}}

WORKDIR /usr/src/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#files}}
  COPY {{.}} {{{.}}}
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

{{#runtime}}
  FROM {{{.}}}
{{/runtime}}
{{^runtime}}
  FROM eclipse-temurin:17.0.6_10-jre
{{/runtime}}


WORKDIR /opt/web

{{#fix}}
  RUN {{{.}}}
{{/fix}}

{{#binaries}}
  COPY --from=build /usr/src/web/{{{.}}} {{{.}}}
{{/binaries}}

RUN ln -sfv /opt/java/openjdk/bin/java /usr/bin/java

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
