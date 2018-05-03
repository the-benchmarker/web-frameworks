FROM swift

WORKDIR /usr/src/app

COPY . .

RUN swift build -c release

CMD .build/release/benchmarker wrk
