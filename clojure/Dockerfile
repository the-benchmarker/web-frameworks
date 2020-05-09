{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM clojure:openjdk-11-tools-deps-slim-buster
{{/image}}

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN clojure -Auberjar
{{/build}}

FROM openjdk:11-jre-slim-buster

WORKDIR /opt/web

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

RUN ln -sfv /usr/local/openjdk-11/bin/java /usr/bin/java

{{#command}}
  CMD {{{.}}}
{{/command}}
