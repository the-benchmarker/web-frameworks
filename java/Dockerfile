FROM maven:3.6-jdk-8 as maven
WORKDIR /usr/src/app

COPY src src
COPY pom.xml pom.xml

<<<<<<< HEAD
{{#build}}
  RUN mvn compile assembly:single -q
{{/build}}

{{^build}}
  RUN mvn clean package -q
{{/build}}
=======
{{#mvn_build}}
  RUN mvn compile assembly:single -q
{{/mvn_build}}

{{^mvn_build}}
  RUN mvn clean package -q
{{/mvn_build}}
>>>>>>> fix(java): Homogenize builds


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
<<<<<<< HEAD
{{/command}}
=======
{{/command}}
>>>>>>> fix(java): Homogenize builds
