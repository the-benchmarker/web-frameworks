FROM swift:4.2

WORKDIR /usr/src/app

COPY Package.swift main.swift ./

RUN swift build -c release

CMD .build/release/server serve --hostname 0.0.0.0 --port 3000 --env prod
