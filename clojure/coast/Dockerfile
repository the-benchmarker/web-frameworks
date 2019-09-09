FROM clojure:tools-deps

WORKDIR /coast

COPY . /coast

ENV COAST_ENV=prod

RUN clj -Auberjar

CMD java -jar coast.jar 3000
