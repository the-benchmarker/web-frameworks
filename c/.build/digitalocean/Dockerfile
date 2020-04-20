FROM alpine 

RUN apk add build-base

{{#build_deps}}
   RUN apk add {{{.}}}
{{/build_deps}}

WORKDIR /usr/src/app

COPY . ./

{{#clone}}
  RUN {{{.}}}
{{/clone}}

{{#build}}
   RUN {{{.}}}
{{/build}}

FROM alpine

{{#bin_deps}}
   RUN apk add {{{.}}}
{{/bin_deps}}

{{#files}}
COPY --from=0 /usr/src/app/{{.}} /usr/bin/{{{.}}}
{{/files}}

{{#command}}
  CMD {{{.}}}
{{/command}}
