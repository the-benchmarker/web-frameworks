FROM swift

WORKDIR /usr/src/app

COPY . .

RUN apt-get install build-essential libssl-dev git -y
RUN git clone https://github.com/wg/wrk.git wrk
RUN cd wrk; make
RUN cp wrk/wrk .

RUN swift build -c release

CMD ./run.sh
