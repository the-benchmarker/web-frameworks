FROM ubuntu:18.04

RUN apt-get update -qq && \
    apt-get install -qq -y \
            wget \
            make \
            gcc \
            openssl \
            libssl-dev && \
    apt-get clean -qq -y && \
    apt-get autoclean -qq -y && \
    apt-get autoremove -qq -y

RUN wget -c https://kore.io/releases/kore-3.1.0.tar.gz && tar xvf kore-3.1.0.tar.gz
RUN cd kore-3.1.0 && TASKS=1 NOTLS=1 make && make install

WORKDIR /usr/src/app

COPY hello hello

WORKDIR /usr/src/app/hello

RUN kodev build

EXPOSE 3000

CMD kore -f -n -r -c conf/hello.conf
