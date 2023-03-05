{{#image}}
  FROM {{{.}}} AS build
{{/image}}

{{^image}}
  FROM maven:3-openjdk-17 AS build
{{/image}}

WORKDIR /usr/src/web

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}
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
  FROM eclipse-temurin:17-jre
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
