FROM bigtruedata/sbt:0.13.15-2.12.3

WORKDIR /usr/src/app

COPY build.sbt ./
COPY src src
COPY project project

RUN sbt assembly

CMD {{command}}
