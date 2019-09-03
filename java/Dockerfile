FROM maven:3.6-jdk-8 as maven
WORKDIR /usr/src/app

COPY src src
COPY pom.xml pom.xml

{{#build}}
  RUN mvn compile assembly:single -q
{{/build}}

{{^build}}
  RUN mvn clean package -q
{{/build}}


FROM openjdk:8-jre-slim
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