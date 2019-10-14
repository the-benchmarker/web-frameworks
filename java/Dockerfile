{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM maven:3-jdk-8
{{/image}}

WORKDIR /usr/src/app

COPY . ./

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN mvn clean package -q
{{/build}}

<<<<<<< HEAD
FROM openjdk:8-alpine

{{#bin_deps}}
  RUN apk add {{{.}}}
{{/bin_deps}}

WORKDIR /usr/src/app


{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}

=======
FROM openjdk:8-jre-slim
WORKDIR /usr/src/app
{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}
>>>>>>> Run into lightweight containers (#1578)
{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#command}}
  CMD {{{.}}}
{{/command}}
