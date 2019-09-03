FROM nimlang/nim:0.20.2

WORKDIR /usr/src/app

COPY . ./

RUN nimble c -d:release --threads:on -y server.nim
