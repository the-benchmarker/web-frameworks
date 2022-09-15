{{#image}}
  FROM {{{.}}} AS build
{{/image}}

{{^image}}
  FROM maven:3-openjdk-17 AS build
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

FROM openjdk:11-jre-slim

WORKDIR /opt/web

RUN ln -sfv /usr/local/openjdk-11/bin/java /usr/bin/java

{{#binaries}}
  COPY --from=build /usr/src/web/{{{.}}} {{{.}}}
{{/binaries}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
