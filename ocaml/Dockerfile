FROM ocaml/opam:debian-12-ocaml-5.2

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}


{{#command}}{{{.}}}{{/command}}