FROM clojure:openjdk-11-tools-deps-slim-buster

WORKDIR /opt/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN clojure -Auberjar