FROM swift

WORKDIR /usr/src/app

COPY Package.swift ./
COPY Sources Sources

RUN swift package update
RUN swift build -c release

CMD .build/release/server
