FROM node:14.16-alpine

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

WORKDIR /usr/src/app

{{#files}}
  COPY {{source}} {{target}}
{{/files}}

{{#bin_deps}}
  RUN apk add {{{.}}}
{{/bin_deps}}

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
