FROM php:7.3.2-fpm

RUN apt-get -qq update
RUN apt-get -y install git nginx zlib1g-dev libzip-dev
RUN docker-php-ext-install zip opcache


WORKDIR /usr/src/app

COPY composer.json .
COPY public public
COPY config config
COPY src src

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative

RUN mkdir /usr/src/app/data
RUN chmod 777 -R /usr/src/app/data
RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.owner.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.group.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.mode.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf

RUN rm -fr /etc/nginx/sites-enabled/default
RUN rm -fr /usr/local/etc/php-fpm.d/zz-docker.conf

ENV APP_ENV prod

RUN echo 'server {\n\
    listen 0.0.0.0:3000;\n\
    root /usr/src/app/public;\n\
    set $front_controller /index.php;\n\
    location / {\n\
        fastcgi_pass unix:/var/run/php-fpm.sock;\n\
        include fastcgi_params;\n\
        fastcgi_param SCRIPT_FILENAME $document_root$front_controller;\n\
        fastcgi_param SCRIPT_NAME $front_controller;\n\
    }\n\
}\n'\
>> /etc/nginx/conf.d/www.conf

RUN echo 'opcache.enable=1\n\
opcache.memory_consumption=512\n\
opcache.interned_strings_buffer=64\n\
opcache.max_accelerated_files=32531\n\
opcache.validate_timestamps=0\n\
opcache.save_comments=1\n\
opcache.fast_shutdown=0\n'\
>> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

RUN echo "daemon off;" >> /etc/nginx/nginx.conf


EXPOSE 3000

CMD /usr/local/sbin/php-fpm --daemonize; service nginx start
