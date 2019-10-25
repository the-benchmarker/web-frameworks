FROM openjdk:8-alpine

WORKDIR /usr/src/app

COPY . ./

{{#deps}}
  RUN apk add {{{.}}}
{{/deps}}

{{^deps}}
  RUN apk add maven
{{/deps}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN mvn clean package -q
{{/build}}

FROM openjdk:8-alpine

{{#bin_deps}}
  RUN apk add {{{.}}}
{{/bin_deps}}

WORKDIR /usr/src/app


{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#command}}
  CMD {{{.}}}
{{/command}}
