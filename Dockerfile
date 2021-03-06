FROM clojure:openjdk-8-lein
COPY project.clj /usr/src/app/
WORKDIR /usr/src/app
RUN lein deps
COPY . /usr/src/app

CMD ["lein", "repl", ":headless"]

