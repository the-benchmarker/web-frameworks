FROM gcc:latest

RUN apt update && apt install -y libgc-dev

ENV CHOOSENIM_NO_ANALYTICS 1
ENV CHOOSENIM_CHOOSE_VERSION #0c683d28bbd5
RUN curl https://nim-lang.org/choosenim/init.sh -sSf | sh -s -- -y
ENV PATH $PATH:/root/.nimble/bin

WORKDIR /usr/src/app

COPY server_nim_jester.nim server_nim_jester.nimble ./

RUN nimble install -y httpbeast@#995fe86443d4a0
RUN nimble c -d:release --threads:on --gc:boehm server_nim_jester.nim

CMD ./server_nim_jester
