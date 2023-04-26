{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM clojure:temurin-17-tools-deps-jammy
{{/image}}

WORKDIR /usr/src/app

COPY . ./

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN clojure -Auberjar
{{/build}}

FROM eclipse-temurin:17.0.7_7-jre

WORKDIR /opt/bin

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} /opt/bin/{{{.}}} 
{{/binaries}}

{{#command}}
  CMD {{{.}}}
{{/command}}
