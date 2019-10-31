FROM julia:1.3

WORKDIR /usr/src/app

COPY server.jl deps.jl ./

RUN julia deps.jl

CMD julia server.jl