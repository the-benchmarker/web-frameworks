FROM ruby

WORKDIR /usr/src/app

COPY Gemfile Gemfile.lock Guardfile config.ru ./
COPY app app
COPY config config
COPY spec spec

RUN bundle install

EXPOSE 3000

CMD [ "bundle", "exec", "rackup", "-p", "3000", "-q", "-E", "production" ]
