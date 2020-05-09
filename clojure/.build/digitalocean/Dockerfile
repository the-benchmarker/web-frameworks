{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM clojure:openjdk-11-tools-deps-slim-buster
{{/image}}

WORKDIR /opt/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN clojure -Auberjar
{{/build}}
