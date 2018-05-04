FROM ruby

WORKDIR /usr/src/app

COPY Gemfile app.rb config.ru ./

RUN bundle install

CMD bundle exec puma -p 8005 -e production -w $(nproc)
