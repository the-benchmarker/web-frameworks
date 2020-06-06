FROM nimlang/nim:1.2.0-alpine AS build

WORKDIR /usr/src/app

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

# hadolint ignore=DL3006
FROM alpine

WORKDIR /opt/web

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}
