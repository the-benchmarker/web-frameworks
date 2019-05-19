FROM zaherg/php-swoole:7.3

WORKDIR /usr/src/app

COPY composer.json .
COPY src src
COPY config config
COPY public public
COPY bin bin

USER root
RUN composer install --no-dev --prefer-dist --classmap-authoritative

ENV APP_ENV prod
RUN mkdir -p var/log && chown www-data var/log
RUN mkdir -p var/cache && chown www-data var/cache
RUN php bin/console cache:warmup

EXPOSE 3000

USER www-data
CMD bin/console swoole:server:run --host=0.0.0.0 --port=3000
