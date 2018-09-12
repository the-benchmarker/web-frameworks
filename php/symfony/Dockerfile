FROM php:fpm

RUN apt-get -qq update
RUN apt-get -y install git nginx zlib1g-dev
RUN docker-php-ext-install zip


WORKDIR /usr/src/app

COPY composer.json .
COPY src src
COPY config config
COPY public public
COPY bin bin

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative

RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.owner.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.group.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.mode.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf

RUN rm -fr /etc/nginx/sites-enabled/default
RUN rm -fr /usr/local/etc/php-fpm.d/zz-docker.conf

ENV APP_ENV prod

RUN mkdir -p var/log && chown www-data var/log
RUN mkdir -p var/cache && chown www-data var/cache
RUN php bin/console cache:warmup

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
