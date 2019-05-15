FROM ibmcom/swift-ubuntu:5.0.1

WORKDIR /usr/src/app

COPY Package.swift main.swift ./

RUN export KITURA_NIO=1 && swift build -c release

CMD .build/release/server
