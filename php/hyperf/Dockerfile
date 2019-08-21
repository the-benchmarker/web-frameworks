FROM php:7.3

RUN apt -y update
RUN apt -y install git

RUN apt -y install libzip-dev
RUN docker-php-ext-install zip

RUN pecl install redis
RUN docker-php-ext-enable redis

RUN pecl install swoole-4.4.1
RUN docker-php-ext-enable swoole

WORKDIR /usr/src/app

COPY app app
COPY sbin sbin
COPY config config
COPY composer.json composer.json
COPY swoole.ini /usr/local/etc/php/conf.d/docker-php-ext-swoole.ini

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o

CMD php sbin/hyperf.php start
