FROM swift:5.0.1

WORKDIR /usr/src/app

COPY Package.swift main.swift ./

RUN apt-get update && apt-get install -y libssl-dev libcurl4-openssl-dev

RUN export KITURA_NIO=1 && swift build -c release

CMD .build/release/server
