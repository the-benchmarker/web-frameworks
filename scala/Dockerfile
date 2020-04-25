FROM hseeberger/scala-sbt:8u252_1.3.10_2.13.2

WORKDIR /usr/src/app

COPY . ./

RUN sbt assembly

FROM openjdk:14-alpine

WORKDIR /usr/src/app

{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}

{{#command}}
  CMD {{{.}}}
{{/command}}
