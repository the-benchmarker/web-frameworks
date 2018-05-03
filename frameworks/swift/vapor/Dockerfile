FROM swift:4.1

WORKDIR /usr/src/app

COPY . .

RUN swift build -c release

CMD .build/release/server serve -e prod -n 0.0.0.0 -p 8001
