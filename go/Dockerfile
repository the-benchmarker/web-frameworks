FROM golang:1.20-alpine

WORKDIR /usr/src/app

WORKDIR /go/src/app

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

 ENV CGO_ENABLED=0
 ENV GOOS=linux
 ENV GOARCH=amd64

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN go get

RUN go build -a -ldflags '-extldflags "-static"' {{#build_tags}} -tags {{{.}}} {{/build_tags}} -o /go/bin/app ./

FROM alpine
{{#environment}}
ENV {{{.}}}
{{/environment}}
COPY --from=0 /go/bin/app /go/bin/app
{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}


{{#environment}}
  ENV {{{.}}}
{{/environment}}



{{#static_files}}
  COPY '{{source}}' '/go/bin/{{target}}'
{{/static_files}}

CMD {{{command}}}
