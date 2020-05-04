FROM clojure:openjdk-11-tools-deps-slim-buster

WORKDIR /usr/src/app

COPY . ./

RUN clojure -Auberjar

FROM openjdk:11-jre-slim-buster

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#files}}
  COPY --from=0 /usr/src/app/{{{.}}} /opt/bin/{{{.}}} 
{{/files}}

{{#command}}
  CMD {{{.}}}
{{/command}}
