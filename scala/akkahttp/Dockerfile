FROM bigtruedata/sbt

WORKDIR /usr/src/app
COPY . .

RUN sbt assembly

EXPOSE 3000
CMD bin/server_scala_akkahttp
