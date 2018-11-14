FROM gcc:8.2

WORKDIR /usr/src/app

RUN wget -c https://github.com/ohler55/agoo-c/tarball/master -O agoo.tar.gz
RUN tar xzf agoo.tar.gz
RUN mv ohler55* agoo-c
RUN cd agoo-c/src && make

COPY Makefile simple.c ./

# build simple app
RUN make

EXPOSE 3000

CMD ./simple
