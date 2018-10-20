FROM ruby:2.5

WORKDIR /usr/src/app

COPY Gemfile app.rb config.ru ./

RUN bundle install

CMD bundle exec puma -p 3000 -e production -w $(nproc)
