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

FROM openjdk:8

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
