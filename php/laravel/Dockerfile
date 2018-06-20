FROM php:fpm

RUN apt-get -qq update
RUN apt-get -y install git nginx zlib1g-dev
RUN docker-php-ext-install zip


WORKDIR /usr/src/app

COPY app app
COPY bootstrap/app.php bootstrap/app.php
COPY composer.json .
COPY routes routes
COPY config config
COPY public/index.php public/index.php
COPY artisan artisan

ENV APP_ENV production
ENV APP_DEBUG false

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o

RUN mkdir bootstrap/cache && chown www-data bootstrap/cache
RUN php artisan cache:clear
RUN php artisan config:cache

RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf

RUN rm -fr /etc/nginx/sites-enabled/default

RUN echo 'server {\n\
    root /usr/src/app/public;\n\
    listen 0.0.0.0:3000;\n\
    location / {\n\
        fastcgi_pass unix:/var/run/php-fpm.sock;\n\
        fastcgi_param   SCRIPT_FILENAME         $document_root/index.php;\n\
        include fastcgi_params;\n\
    }\n\
}\n'\
>> /etc/nginx/conf.d/www.conf

RUN echo "daemon off;" >> /etc/nginx/nginx.conf

EXPOSE 3000

CMD /usr/local/sbin/php-fpm --daemonize; service nginx start
