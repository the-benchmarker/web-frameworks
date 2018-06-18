FROM php:fpm

RUN apt-get -qq update
RUN apt-get -y install git nginx

WORKDIR /usr/src/app

COPY composer.json .
COPY src src
COPY config config
COPY public public

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative

RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf

RUN rm -fr /etc/nginx/sites-enabled/default

ENV SYMFONY_ENV prod

RUN mkdir -p /usr/src/app/var/log && chown www-data /usr/src/app/var/log
RUN mkdir -p /usr/src/app/var/cache/prod && chown www-data /usr/src/app/var/cache/prod

RUN echo 'server {\n\
    root /usr/src/app/public;\n\
    listen 0.0.0.0:3000;\n\
    location / {\n\
        fastcgi_pass 127.0.0.1:9000;\n\
        fastcgi_param   SCRIPT_FILENAME         $document_root/index.php;\n\
        include fastcgi_params;\n\
    }\n\
}\n'\
>> /etc/nginx/conf.d/www.conf

RUN echo "daemon off;" >> /etc/nginx/nginx.conf

EXPOSE 3000

CMD /usr/local/sbin/php-fpm --daemonize; service nginx start
