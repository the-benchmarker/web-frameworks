FROM julia:1.3

WORKDIR /usr/src/app

COPY merly/server.jl merly/Manifest.toml merly/Project.toml merly/deps.jl /usr/src/app/

RUN julia deps.jl

CMD julia server.jl