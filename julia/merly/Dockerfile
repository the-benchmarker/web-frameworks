FROM julia:1.0.0-stretch

RUN mkdir /usr/src/app
WORKDIR /usr/src/app

RUN julia -e 'using Pkg;Pkg.add("Merly")'

COPY server.jl ./

EXPOSE 3000

CMD julia server.jl