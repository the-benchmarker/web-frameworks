{{#language.version}}
  FROM golang:{{{.}}} AS build
{{/language.version}}
{{^language.version}}
  FROM golang:1.26 AS build
{{/language.version}}

WORKDIR /go/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    zlib1g-dev && \
  rm -rf /var/lib/apt/lists/*

ENV CGO_ENABLED=1 \
  GOOS=linux \
  GOARCH={{#architecture}}{{{.}}}{{/architecture}}

{{#if.architecture.amd64}}
  # This compile with v3 micoarchitecture, since used proc is zen2 compatible ( AMD Ryzen 7 5700U)
  # See https://en.wikipedia.org/wiki/X86-64#Microarchitecture_levels
  ENV GOAMD64=v3
{{/if.architecture.amd64}}

{{#if.architecture.arm64}}
  # Use arm 8.5 instrctions set
  # See https://en.wikipedia.org/wiki/Apple_M1
  ENV GOARM64=v8.5
{{/if.architecture.arm64}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN --mount=type=cache,id=gomod-{{language.version}},target=/go/pkg/mod \
  go get

RUN --mount=type=cache,id=gomod-{{language.version}},target=/go/pkg/mod \
  --mount=type=cache,id=gobuild-{{language.version}},target=/root/.cache/go-build \
  go build -trimpath {{#build_tags}} -tags {{{.}}} {{/build_tags}} -o /go/bin/app ./



FROM debian:trixie-slim

WORKDIR /go/bin

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    libstdc++6 \
    zlib1g && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /go/bin/app /go/bin/app

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
