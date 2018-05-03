FROM swift

WORKDIR /usr/src/app

COPY Package.swift main.swift ./

RUN swift build -c release

CMD .build/release/server serve -e prod -h 0.0.0.0 -p 3000
