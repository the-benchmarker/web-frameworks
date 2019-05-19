FROM php:7.3-cli

RUN apt-get update && apt-get install -y \
        procps \
        openssl \
        libssh-dev \
        libpcre3 \
        libpcre3-dev \
        libnghttp2-dev \
        git \
        curl && \
        apt-get autoremove && apt-get clean

ARG SWOOLE_VERSION=4.3.4
RUN set -ex \
    && pecl install -f swoole-$SWOOLE_VERSION \
    && docker-php-ext-enable swoole

RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

WORKDIR /usr/src/app

COPY composer.json .
COPY public public

ENV COMPOSER_ALLOW_SUPERUSER 1
RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative

ENTRYPOINT php /usr/src/app/public/index.php start

EXPOSE 3000