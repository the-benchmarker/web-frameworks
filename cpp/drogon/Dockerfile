FROM gcc:9.1.0

RUN apt -qq update && \
    apt -qy install cmake libjsoncpp-dev uuid-dev libssl-dev zlib1g-dev

ENV IROOT=/install
ENV DROGON_ROOT=$IROOT/drogon
WORKDIR $IROOT

COPY ./ ./

# compilation

RUN git clone --branch v1.0.0-beta6 https://github.com/an-tao/drogon
WORKDIR $DROGON_ROOT
RUN git submodule update --init

WORKDIR $IROOT/build
RUN cmake -DCMAKE_BUILD_TYPE=release ..
RUN make
CMD ./drogon_benchmark ../config.json

