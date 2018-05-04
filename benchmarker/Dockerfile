FROM swift

WORKDIR /usr/src/app

COPY . .
COPY ../frameworks.json .

RUN swift build -c release

CMD ./run.sh
