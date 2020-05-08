FROM nimlang/nim:1.2.0-alpine

WORKDIR /opt/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

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
