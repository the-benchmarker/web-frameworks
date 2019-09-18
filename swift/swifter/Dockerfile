FROM swift:5.0

RUN apt-get -qq update
RUN apt-get -qy install libssl-dev zlib1g-dev
WORKDIR /usr/src/app

COPY Package.swift main.swift ./

RUN swift build -c release

CMD .build/release/server
