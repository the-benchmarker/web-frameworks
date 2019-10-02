FROM nimlang/nim:1.0.0

WORKDIR /usr/src/app

COPY server.nim server.nimble ./

RUN nimble c -d:release --threads:on -y server.nim

CMD ./server
