FROM swift:4.2

# install kitura deps
# @see http://www.kitura.io/en/starter/settingup.html
RUN apt-get -qq update
RUN apt-get -qy install clang libicu-dev libcurl4-openssl-dev libssl-dev
WORKDIR /usr/src/app

COPY Package.swift main.swift ./

RUN swift build -c release

CMD .build/release/server
