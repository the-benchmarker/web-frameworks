FROM php:7.3-fpm

RUN apt -y update
RUN apt -y install git

RUN pecl install swoole-4.4.5
RUN docker-php-ext-enable swoole

WORKDIR /usr/src

COPY App App
COPY composer.json composer.json


RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o

CMD php App/swoole.php
