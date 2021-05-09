FROM julia:1.6

WORKDIR /usr/src/app

{{#files}}
  COPY {{source}} {{target}}
{{/files}}

RUN julia deps.jl

CMD julia server.jl
