FROM hseeberger/scala-sbt:11.0.7_1.3.10_2.13.2

WORKDIR /usr/src/app

COPY . ./

RUN sbt assembly

# Use the latest stable JVM
# @see https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html
FROM openjdk:14-alpine

WORKDIR /usr/src/app

{{#files}}
COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/files}}

{{#command}}
  CMD {{{.}}}
{{/command}}
