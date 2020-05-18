FROM hseeberger/scala-sbt:11.0.7_1.3.10_2.13.2

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN sbt assembly

FROM openjdk:11-jre

WORKDIR /opt/web

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

RUN ln -sfv /usr/local/openjdk-11/bin/java /usr/bin/java

CMD {{#command}}{{{.}}}{{/command}}
