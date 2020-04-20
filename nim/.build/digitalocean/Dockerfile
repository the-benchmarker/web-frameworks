FROM nimlang/nim:1.0.6-alpine

WORKDIR /usr/src/app

COPY server.nim server.nimble ./

RUN nimble c \
    -d:danger \
    -d:noSignalHandler \
    --gc:markAndSweep \
    --listFullPaths:off \
    --excessiveStackTrace:off \
    --passL:-static \
    --passL:"-s" \
    --passC:"-fno-ident" \
    --passC:"-flto" \
    --passC:"-ffast-math" \
    --passC:"-fsingle-precision-constant" \
    -d:release \
    --threads:on \
        -y server.nim

FROM alpine
COPY --from=0 /usr/src/app/server /usr/src/app/server
CMD /usr/src/app/server
