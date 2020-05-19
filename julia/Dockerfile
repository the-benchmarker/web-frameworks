FROM julia:1.5

WORKDIR /opt/web

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

RUN julia deps.jl

RUN ln -sfv /usr/local/julia/bin/julia /usr/bin/julia

CMD {{#command}}{{{.}}}{{/command}}
