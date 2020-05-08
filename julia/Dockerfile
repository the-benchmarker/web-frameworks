FROM julia:1.5

WORKDIR /opt/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN julia deps.jl

CMD julia server.jl
