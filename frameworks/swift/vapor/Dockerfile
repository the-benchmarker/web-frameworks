FROM swift

WORKDIR /usr/src/app

COPY . .

RUN swift build -c release

CMD .build/release/server serve -e prod -h 0.0.0.0 -p 8001
