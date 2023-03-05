{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM clojure:temurin-17-tools-deps
{{/image}}

WORKDIR /usr/src/app

COPY . ./

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN clojure -Auberjar
{{/build}}

FROM eclipse-temurin:17-jre

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
