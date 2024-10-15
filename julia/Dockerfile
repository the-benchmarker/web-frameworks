{{#language.version}}
  FROM julia:{{{.}}}
{{/language.version}}
{{^language.version}}
  FROM julia:1.11
{{/language.version}}

WORKDIR /usr/src/app

COPY server.jl Project.toml deps.jl ./

RUN julia deps.jl

CMD {{command}}
