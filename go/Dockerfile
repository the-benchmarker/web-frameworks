FROM golang:1.21

WORKDIR /go/src/app

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

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

ENTRYPOINT {{{command}}}
