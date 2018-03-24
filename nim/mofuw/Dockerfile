FROM nimlang/nim

WORKDIR /usr/src/app

COPY server_nim_mofuw.nimble ./
RUN nimble install -y

COPY . .

RUN nim c -d:release server_nim_mofuw.nim

EXPOSE 3000

CMD [ "./server_nim_mofuw" ]