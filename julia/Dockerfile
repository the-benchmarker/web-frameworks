FROM julia:1.3

WORKDIR /usr/src/app

RUN julia -e 'using Pkg;Pkg.add("Merly")'

COPY server.jl ./

CMD julia server.jl