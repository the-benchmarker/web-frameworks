{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM maven:3-jdk-8-alpine
{{/image}}

WORKDIR /usr/src/app

COPY . ./

{{#patch}}
  {{{.}}}
{{/patch}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN mvn clean package -q
{{/build}}


FROM openjdk:8-alpine

{{#bin_deps}}
  RUN apk add {{{.}}}
{{/bin_deps}}

WORKDIR /usr/src/app


{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}


WORKDIR /usr/src/app
{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#command}}
  CMD {{{.}}}
{{/command}}
