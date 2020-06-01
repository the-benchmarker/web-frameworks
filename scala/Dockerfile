FROM hseeberger/scala-sbt:11.0.7_1.3.12_2.13.2

WORKDIR /usr/src/app

COPY . ./

RUN sbt assembly

FROM openjdk:11-jre

WORKDIR /usr/src/app

{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}

{{#command}}
  CMD {{{.}}}
{{/command}}
