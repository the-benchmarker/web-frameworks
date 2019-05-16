FROM gcc:9.1.0

RUN wget -c https://kore.io/releases/kore-3.2.2.tar.gz && tar xvf kore-3.2.2.tar.gz
RUN cd kore-3.2.2 && TASKS=1 NOTLS=1 make && make install

WORKDIR /usr/src/app

COPY hello hello

WORKDIR /usr/src/app/hello

RUN kodev build

EXPOSE 3000

CMD kore -f -n -r -c conf/hello.conf
