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

RUN mkdir -p target
RUN mkdir -p build

FROM openjdk:8-jre-alpine

WORKDIR /opt/web

COPY --from=0 /usr/src/web/target target
COPY --from=0 /usr/src/web/build build

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
