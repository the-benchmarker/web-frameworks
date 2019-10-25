FROM openjdk:8-alpine

RUN apk add maven

WORKDIR /usr/src/app

COPY . ./

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN mvn clean package -q
{{/build}}

FROM openjdk:8-alpine

WORKDIR /usr/src/app

{{#deps}}
  RUN apk add {{{.}}}
{{/deps}}

{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#command}}
  CMD {{{.}}}
{{/command}}
