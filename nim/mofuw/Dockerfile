FROM gcc:latest

RUN apt update -yqq &&                                                  \
    mkdir -p /nim &&                                                    \
    cd /nim &&                                                          \
    git clone -b devel https://github.com/nim-lang/Nim.git nim-devel && \
    cd nim-devel &&                                                     \
    git clone --depth 1 https://github.com/nim-lang/csources.git &&     \
    cd csources &&                                                      \
    sh build.sh &&                                                      \
    cd ../ &&                                                           \
    bin/nim c koch &&                                                   \
    ./koch boot -d:release &&                                           \
    ./koch tools

ENV PATH $PATH:/nim/nim-devel/bin:/root/.nimble/bin

COPY server_nim_mofuw.nim ./

RUN nimble install -y mofuw 
RUN nim c -d:release --threads:on -d:bufSize:512 server_nim_mofuw.nim

CMD [ "./server_nim_mofuw" ]
