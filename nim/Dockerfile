FROM nimlang/nim:1.6.4-alpine

WORKDIR /usr/src/app

COPY . ./

RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories
RUN apk update && \
    apk upgrade --no-cache

{{#deps}}
  RUN apk --no-cache add {{{.}}}
{{/deps}}

ENV PATH $PATH:/root/.nimble/bin

RUN nimble install -y

RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --excessiveStackTrace:off \
    -d:release \
    --passC:-flto \
    --passL:-flto \
    server.nim

FROM alpine

COPY --from=0 /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server
