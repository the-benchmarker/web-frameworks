FROM clojure:tools-deps

WORKDIR /usr/src/app

COPY . /usr/src/app

RUN COAST_ENV=prod clj -Auberjar

EXPOSE 1337

CMD COAST_ENV=prod java -jar coast.jar 1337
