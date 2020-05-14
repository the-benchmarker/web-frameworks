FROM gradle:jdk11 As build

WORKDIR /usr/src/app

USER root
RUN chown -R gradle .
USER gradle

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN gradle build

FROM openjdk:11-jre-slim

WORKDIR /opt/web

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} {{{.}}}}
{{/binaries}}

RUN ln -sfv /usr/local/openjdk-11/bin/java /usr/bin/java

CMD {{#command}}{{{.}}}{{/command}}

