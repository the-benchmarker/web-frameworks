FROM ruby:2.3

RUN apt-get update \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/app
COPY Gemfile* ./
RUN bundle install
COPY . .

CMD ["rails", "s", "-b", "0.0.0.0", "-e", "production"]