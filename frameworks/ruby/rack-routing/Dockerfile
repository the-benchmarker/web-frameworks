FROM ruby

WORKDIR /usr/src/app

COPY Gemfile Guardfile config.ru ./
COPY app app
COPY config config
COPY spec spec

RUN bundle install

EXPOSE 3000

CMD bundle exec puma -p 3000 -e production -w $(nproc)
