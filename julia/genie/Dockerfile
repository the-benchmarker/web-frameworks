FROM julia:1.1

WORKDIR /usr/src/app

COPY deps.jl server.jl ./

RUN julia deps.jl

CMD julia server.jl
