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

{{#sources}}
  COPY {{.}} {{{.}}}
{{/sources}}

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
  FROM openjdk:17.0.2-slim
{{/runtime}}


WORKDIR /opt/web

{{#fix}}
  RUN {{{.}}}
{{/fix}}

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
