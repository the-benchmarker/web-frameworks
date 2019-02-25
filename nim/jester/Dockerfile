FROM nimlang/nim:0.19.4

WORKDIR /usr/src/app

COPY server_nim_jester.nim server_nim_jester.nimble ./

RUN nimble install -y httpbeast@#v0.2.1
RUN nimble c -d:release --threads:on -y server_nim_jester.nim

CMD ./server_nim_jester
