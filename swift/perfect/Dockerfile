FROM swift:4.2

# install perfect deps
# @see https://www.perfect.org/docs/gettingStarted.html
RUN apt-get -qq update
RUN apt-get -qy install openssl libssl-dev uuid-dev

WORKDIR /usr/src/app

COPY Package.swift main.swift ./

RUN swift build -c release

CMD .build/release/server
