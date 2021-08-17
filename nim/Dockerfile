FROM nimlang/nim:1.4.8-alpine

WORKDIR /usr/src/app

COPY server.nim server.nimble server.nim.cfg ./


ENV PATH $PATH:/root/.nimble/bin

RUN nimble install -y

# RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
#     --excessiveStackTrace:off \
#     -d:release \
#     --passC:-flto \
#     --passL:-flto \
#     server.nim

# FROM alpine

# COPY --from=0 /usr/src/app/server /usr/src/app/server

# CMD /usr/src/app/server
