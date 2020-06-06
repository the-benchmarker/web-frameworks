FROM golang:1.14 AS build

ENV CGO_ENABLED 0
ENV GOOS linux
ENV GOARCH amd64

WORKDIR /go/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#boostrap}}
  RUN {{{.}}}
{{/boostrap}}

RUN go get

RUN go build -a -ldflags '-extldflags "-static"' -o /go/bin/app ./

# hadolint ignore=DL3006
FROM alpine

WORKDIR /opt/web

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#binaries}}
  COPY --from=build /go/bin/{{{.}}} {{{.}}}
{{/binaries}}
