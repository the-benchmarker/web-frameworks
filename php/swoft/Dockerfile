FROM php:7.3.8-fpm

RUN apt -y update
RUN apt -y install git

RUN pecl install redis
RUN docker-php-ext-enable redis

RUN pecl install swoole-4.4.1
RUN docker-php-ext-enable swoole

WORKDIR /usr/src/app

COPY app app
COPY bin bin
COPY composer.json composer.json
COPY config config

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o

CMD php bin/swoft http:start
