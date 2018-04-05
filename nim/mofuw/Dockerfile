FROM nimlang/nim

WORKDIR /usr/src/app
COPY . .
RUN nimble install -y
RUN nim c server_nim_mofuw.nim
