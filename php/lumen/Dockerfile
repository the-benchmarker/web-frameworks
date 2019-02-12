FROM php:7.3.2-fpm

RUN apt-get -qq update
RUN apt-get -y install git nginx zlib1g-dev libzip-dev
RUN docker-php-ext-install zip opcache


WORKDIR /usr/src/app

COPY app app
COPY bootstrap/app.php bootstrap/app.php
COPY composer.json .
COPY routes routes
COPY public/index.php public/index.php

ENV APP_ENV production
ENV APP_DEBUG false
ENV APP_KEY base64:txfHNf/SOo222Rm8I39Urb9SmvUy+nuAF98t/ukF0lk=

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o

RUN mkdir bootstrap/cache
RUN mkdir storage/framework/sessions -p
RUN mkdir storage/framework/views -p
RUN mkdir storage/framework/cache -p

RUN chown -R www-data:www-data /usr/src/app
RUN find /usr/src/app -type f -exec chmod 644 {} \;
RUN find /usr/src/app -type d -exec chmod 755 {} \;
RUN chmod -R ug+rwx storage /usr/src/app/bootstrap/cache

RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.owner.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.group.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's/\;\(listen\.mode.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf


RUN rm -fr /etc/nginx/sites-enabled/default
RUN rm -fr /usr/local/etc/php-fpm.d/zz-docker.conf

RUN echo 'server {\n\
    root /usr/src/app/public;\n\
    listen 0.0.0.0:3000;\n\
    index index.php;\n\
\n\
    add_header X-Frame-Options "SAMEORIGIN";\n\
    add_header X-XSS-Protection "1; mode=block";\n\
    add_header X-Content-Type-Options "nosniff";\n\
\n\
    charset utf-8;\n\
\n\
    location / {\n\
        try_files $uri $uri/ /index.php?$query_string;\n\
    }\n\
\n\
    location ~ \.php$ {\n\
        fastcgi_split_path_info ^(.+\.php)(/.+)$;\n\
        fastcgi_pass unix:/var/run/php-fpm.sock;\n\
        fastcgi_index index.php;\n\
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;\n\
        include fastcgi_params;\n\
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
