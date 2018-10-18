FROM nimlang/nim:0.19.0

COPY server_nim_mofuw.nim ./

RUN nimble install -y mofuw@2.0.0
RUN nim c -d:release --threads:on -d:bufSize:512 server_nim_mofuw.nim

CMD [ "./server_nim_mofuw" ]

