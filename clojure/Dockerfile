{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM clojure:openjdk-11-tools-deps-slim-buster
{{/image}}

WORKDIR /usr/src/app

COPY . ./

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN clojure -Auberjar
{{/build}}

FROM openjdk:11.0.16-jre-slim-buster

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} /opt/bin/{{{.}}} 
{{/binaries}}

RUN ln -sfv /usr/local/openjdk-11/bin/java /usr/bin/java

{{#command}}
  CMD {{{.}}}
{{/command}}
