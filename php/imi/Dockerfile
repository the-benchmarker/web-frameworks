FROM php:7.3-fpm

RUN apt -y update
RUN apt -y install git

RUN pecl install redis
RUN docker-php-ext-enable redis

RUN pecl install swoole-4.4.5
RUN docker-php-ext-enable swoole

WORKDIR /usr/src

COPY .runtime .runtime
COPY ApiServer ApiServer
COPY config config
COPY composer.json composer.json
COPY Main.php Main.php

RUN chmod -R ug+rwx /usr/src/app/.runtime

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o

CMD php vendor/bin/imi server/start
