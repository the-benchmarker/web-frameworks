{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM maven:3-jdk-8-alpine
{{/image}}

WORKDIR /usr/src/app

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
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

{{#after_command}}
  RUN {{{.}}}
{{/after_command}}

{{#deps}}
  RUN apk add {{{.}}}
{{/deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#command}}
  CMD {{{.}}}
{{/command}}

{{^command}}
  CMD {{{.}}}
{{/command}}
