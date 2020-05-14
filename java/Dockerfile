{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM maven:3-jdk-8-alpine
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


FROM openjdk:8-jre-alpine

WORKDIR /opt/web

{{#binaries}}
  COPY --from=0 /usr/src/web/{{{.}}} {{{.}}}
{{/binaries}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#alpine-deps}}
  RUN apk add {{{.}}}
{{/alpine-deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{#command}}{{{.}}}{{/command}}
