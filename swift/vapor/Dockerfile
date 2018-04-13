FROM swift

WORKDIR /usr/src/app

COPY . .
RUN swift package update
RUN swift build -c release

CMD .build/release/server_swift_vapor
