FROM golang:1.20-alpine

WORKDIR /usr/src/app

WORKDIR /go/src/app
COPY . ./

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

 ENV CGO_ENABLED=0
 ENV GOOS=linux
 ENV GOARCH=amd64

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

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



RUN apk add build-base {{#deps}}{{{.}}}{{/deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#static_files}}
  COPY '{{source}}' '/go/bin/{{target}}'
{{/static_files}}

CMD {{{command}}}
