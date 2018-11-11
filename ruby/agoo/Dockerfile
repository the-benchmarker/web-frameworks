FROM ruby:2.5.3

WORKDIR /usr/src/app

COPY Gemfile app.rb ./

RUN bundle install

CMD AGOO_WORKER_COUNT=$(nproc) ruby app.rb
