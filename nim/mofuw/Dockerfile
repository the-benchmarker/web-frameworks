FROM nimlang/nim

WORKDIR /usr/src/app

COPY server_nim_mofuw.nim server_nim_mofuw.nim.cfg server_nim_mofuw.nimble ./

RUN nimble install -y
RUN nim c server_nim_mofuw.nim

CMD [ "./server_nim_mofuw" ]
