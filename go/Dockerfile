FROM golang:1.18-alpine

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

WORKDIR /go/src/app
COPY . ./

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

RUN go get

RUN go build -a -ldflags '-extldflags "-static"' -o /go/bin/app ./

FROM alpine
{{#environment}}
ENV {{{.}}}
{{/environment}}
COPY --from=0 /go/bin/app /go/bin/app
{{#files}}
COPY --from=0 /go/src/app/{{{.}}} /go/bin/{{{.}}}
{{/files}}
CMD cd /go/bin && ./app
