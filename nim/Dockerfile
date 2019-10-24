FROM nimlang/nim:1.0.0

RUN apt -y update && \
    apt -y install musl-tools

WORKDIR /usr/src/app

COPY server.nim server.nimble ./

RUN nimble c \
    -d:danger \
    -d:noSignalHandler \
    --gc:markAndSweep \
    --listFullPaths:off \
    --excessiveStackTrace:off \
    --gcc.exe:musl-gcc \
    --gcc.linkerexe:musl-gcc \
    --passL:-static \
    --passL:"-s" \
    --passC:"-fno-ident" \
    --passC:"-flto" \
    --passC:"-ffast-math" \
    --passC:"-march=native" \
    --passC:"-mtune=native" \
    --passC:"-fsingle-precision-constant" \
    -d:release \
    --threads:on \
        -y server.nim

FROM alpine
COPY --from=0 /usr/src/app/server /usr/src/app/server
CMD /usr/src/app/server
