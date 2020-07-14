FROM golang:1.15-alpine

ENV CGO_ENABLED 0
ENV GOOS linux
ENV GOARCH amd64

WORKDIR /go/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN go get

RUN go build -a -ldflags '-extldflags "-static"' -o /go/bin/app ./

FROM alpine

WORKDIR /opt/web

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

{{#binaries}}
  COPY --from=0 /go/bin/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}
