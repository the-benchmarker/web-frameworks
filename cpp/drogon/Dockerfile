FROM gcc:9.1.0

RUN apt -qq update && \
    apt -qy install cmake libjsoncpp-dev uuid-dev libssl-dev zlib1g-dev

ENV IROOT=/install
ENV DROGON_ROOT=$IROOT/drogon
ENV TEST_PATH=/drogon_benchmark/build
WORKDIR $IROOT

COPY ./ ./

# compilation

RUN git clone --branch v1.0.0-beta2 https://github.com/an-tao/drogon
WORKDIR $DROGON_ROOT
RUN git submodule update --init
WORKDIR $DROGON_ROOT/build
RUN cmake ..
RUN make
RUN make install

WORKDIR $IROOT/build
RUN cmake ..
RUN make
CMD ./drogon_benchmark config.json

