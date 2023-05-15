FROM julia:1.9

WORKDIR /usr/src/app

COPY server.jl Project.toml deps.jl ./

RUN julia deps.jl

CMD {{command}}
