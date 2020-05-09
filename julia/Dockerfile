FROM julia:1.5

WORKDIR /opt/web

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

RUN julia deps.jl

CMD julia server.jl
