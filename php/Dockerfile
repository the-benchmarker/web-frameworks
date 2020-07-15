FROM php:7.4-fpm-alpine

RUN apk add git zlib-dev libzip-dev build-base autoconf nginx openrc {{#alpine-deps}} {{{.}}} {{/alpine-deps}}
RUN docker-php-ext-install zip opcache bcmath

WORKDIR /opt/web

{{#php_ext}}
  RUN pecl install {{{.}}}
  RUN docker-php-ext-enable {{{.}}}
{{/php_ext}}

{{#php_mod}}
  RUN docker-php-ext-install {{{.}}}
{{/php_mod}}

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

# Create symlink for php-fpm config to mimic digitalocean
RUN mkdir -p /etc/php-fpm.d
RUN ln -sfv /usr/local/etc/php-fpm.d/www.conf /etc/php-fpm.d/www.conf

# Create symlink for php
RUN ln -sfv /usr/local/bin/php /usr/bin/php

# Create php-fpm nginx config
RUN echo 'upstream php-fpm {server 127.0.0.1:9000;}' > /etc/nginx/conf.d/php-fpm.conf

# Create directory to store nginx pid
RUN mkdir /run/nginx

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#command}}
  CMD {{{.}}}
{{/command}}

{{^command}}
  CMD /usr/local/sbin/php-fpm --daemonize; nginx -g "daemon off;"
{{/command}}
