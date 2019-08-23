FROM nimlang/nim:0.20.0

WORKDIR /usr/src/app

COPY server_nim_jester.nim server_nim_jester.nimble ./

RUN nimble c -d:release --threads:on -y server_nim_jester.nim

CMD ./server_nim_jester
