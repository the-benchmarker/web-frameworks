FROM clojure:tools-deps

WORKDIR /coast

COPY . /coast

ENV COAST_ENV=prod

<<<<<<< HEAD
RUN clojure -Auberjar
=======
RUN clj -Auberjar
>>>>>>> fix(clojure): Homogenize builds

CMD java -jar coast.jar 3000
