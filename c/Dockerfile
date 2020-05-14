FROM alpine 

RUN apk add build-base

{{#build_deps}}
   RUN apk add {{{.}}}
{{/build_deps}}

WORKDIR /usr/src/app

{{#sources}}
   COPY {{{.}}} {{{.}}}
{{/sources}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

FROM alpine

WORKDIR /opt/web

{{#bin_deps}}
   RUN apk add {{{.}}}
{{/bin_deps}}

{{#files}}
   COPY {{{.}}} {{{.}}}
{{/files}}

{{#binaries}}
   COPY --from=0 /usr/src/app/{{.}} {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}
