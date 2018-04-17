FROM swift

WORKDIR /usr/src/app

COPY Package.swift Package.resolved ./
COPY Sources Sources

RUN swift package update
RUN swift build -c release

CMD .build/release/server
