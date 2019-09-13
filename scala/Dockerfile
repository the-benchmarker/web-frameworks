FROM bigtruedata/sbt:0.13.15-2.12.3

WORKDIR /usr/src/app

COPY build.sbt ./
COPY src src
COPY project project

RUN sbt assembly

<<<<<<< HEAD
CMD java -jar target/scala-2.12/*assembly*.jar
=======
# FROM alpine
# COPY --from=0 

CMD {{command}}
>>>>>>> fix(scala): Homogenize builds
