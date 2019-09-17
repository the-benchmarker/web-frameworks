FROM php:7.3

RUN apt-get -qy update

WORKDIR /usr/src/app

COPY composer.json composer.json
COPY app app
COPY app.php app.php

# install composer
RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
# php/zip extension is required for composer
RUN apt -y update && \
    apt -y install zlib1g-dev libzip-dev
RUN docker-php-ext-install zip

RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o
# dumping autoloader require php/opcache
#RUN docker-php-ext-install opcache

# install spiral binary file
RUN vendor/bin/spiral get

# configure
RUN php app.php configure

# run roadrunner
CMD php app.php up > /dev/null 2>&1 && ./spiral serve -o "http.workers.pool.numWorkers = 64"
