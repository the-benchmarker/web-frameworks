FROM ruby:2.6

WORKDIR /usr/src/app

COPY Gemfile config.ru ./

RUN bundle install

EXPOSE 3000

CMD bundle exec puma -p 3000 -e production -w $(nproc)
