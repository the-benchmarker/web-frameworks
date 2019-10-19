FROM julia:1.0.0-stretch

RUN mkdir /usr/src/app
WORKDIR /usr/src/app

COPY server.jl ./

RUN julia -e 'using Pkg;Pkg.add("Merly")'

EXPOSE 3000

CMD julia server.jl