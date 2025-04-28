{{#language.version}}
  FROM golang:{{{.}}}
{{/language.version}}

{{^language.version}}
  FROM golang:1.24
{{/language.version}}

WORKDIR /go/src/app

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH={{#architecture}}{{{.}}}{{/architecture}}

{{#if.architecture.amd64}}
  # This compile with v2 micoarchitecture
  # See https://en.wikipedia.org/wiki/X86-64#Microarchitecture_levels 
  ENV GOAMD64=v2
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

RUN go get
RUN go build -a -ldflags '-extldflags "-static"' {{#build_tags}} -tags {{{.}}} {{/build_tags}} -o /go/bin/app ./


FROM debian:stable

WORKDIR /go/bin

{{#environment}}
ENV {{{.}}}
{{/environment}}

COPY --from=0 /go/bin/app /go/bin/app

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
