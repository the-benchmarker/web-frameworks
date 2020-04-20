FROM hseeberger/scala-sbt:8u222_1.3.3_2.13.1

WORKDIR /usr/src/app

COPY . ./

RUN sbt assembly

FROM openjdk:13-alpine

WORKDIR /usr/src/app

{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}

{{#command}}
  CMD {{{.}}}
{{/command}}
