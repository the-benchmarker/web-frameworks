FROM swift

WORKDIR /usr/src/app

COPY . /usr/src/app
RUN swift build -c release

CMD .build/release/server_swift_vapor
