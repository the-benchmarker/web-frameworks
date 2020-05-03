FROM hseeberger/scala-sbt:11.0.7_1.3.10_2.13.2

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN sbt assembly

FROM openjdk:11-jre

WORKDIR /usr/src/app

{{#binaries}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

{{#command}}
  CMD {{{.}}}
{{/command}}
