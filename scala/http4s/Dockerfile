FROM bigtruedata/sbt

WORKDIR /usr/src/app

COPY build.sbt ./
COPY src src
COPY project project

RUN sbt assembly

EXPOSE 3000

CMD java -jar target/scala-2.12/*assembly*.jar
