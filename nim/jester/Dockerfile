FROM nimlang/nim

WORKDIR /usr/src/app

COPY server_nim_jester.nimble ./
RUN nimble install -y

COPY . .

RUN nim c server_nim_jester.nim

EXPOSE 3000

CMD [ "./server_nim_jester" ]