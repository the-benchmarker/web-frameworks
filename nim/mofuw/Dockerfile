FROM nimlang/nim

WORKDIR /usr/src/app

COPY nest.nim server_nim_mofuw.nim server_nim_mofuw.nim.cfg server_nim_mofuw.nimble ./

RUN nimble install -y
RUN nim c -d:release -d:bufSize:512 server_nim_mofuw.nim

CMD [ "./server_nim_mofuw" ]
