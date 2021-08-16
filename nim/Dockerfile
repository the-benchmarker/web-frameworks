FROM nimlang/nim:1.4.8

WORKDIR /usr/src/app

COPY . ./


ENV PATH $PATH:/root/.nimble/bin

RUN nimble install -y

RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --excessiveStackTrace:off \
    -d:release \
    --passC:-flto \
    --passL:-flto \
    server.nim

FROM ubuntu

COPY --from=0 /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server
