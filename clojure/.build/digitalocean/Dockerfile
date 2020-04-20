FROM clojure:openjdk-14-tools-deps-1.10.1.478-alpine

WORKDIR /usr/src/app

COPY . ./

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

RUN clojure -Auberjar

FROM openjdk:14-alpine

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
