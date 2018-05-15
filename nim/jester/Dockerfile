FROM nimlang/nim

WORKDIR /usr/src/app

COPY server_nim_jester.nim server_nim_jester.nim.cfg server_nim_jester.nimble ./

RUN nimble install -y
RUN nim c -d:release server_nim_jester.nim

CMD [ "./server_nim_jester" ]
