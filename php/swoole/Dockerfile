FROM ldjbenben/swoole:4.3-php7.3-cli-composer

WORKDIR /usr/src/app

COPY composer.json .
COPY public public

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative

ENTRYPOINT php /usr/src/app/public/index.php start

EXPOSE 3000