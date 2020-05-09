FROM gradle:jdk11

WORKDIR /opt/web

USER root
RUN chown -R gradle /opt/web
USER gradle

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN gradle build
{{/build}}
