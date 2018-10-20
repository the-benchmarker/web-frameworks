FROM php:7.2-fpm

RUN apt-get -qq update
RUN apt-get -y install git nginx zlib1g-dev
RUN docker-php-ext-install zip


WORKDIR /usr/src/app

COPY composer.json .
COPY public public

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

RUN echo 'server {\n\
    root /usr/src/app/public;\n\
    listen 0.0.0.0:3000;\n\
    location / {\n\
        try_files $uri /index.php$is_args$args;\n\
    }\n\
    location ~ index\\.php {\n\
        fastcgi_pass unix:/var/run/php-fpm.sock;\n\
        fastcgi_param   SCRIPT_FILENAME         $document_root/index.php;\n\
        include fastcgi_params;\n\
        internal;\n\
    }\n\
}\n'\
>> /etc/nginx/conf.d/www.conf

RUN echo "daemon off;" >> /etc/nginx/nginx.conf

EXPOSE 3000

CMD /usr/local/sbin/php-fpm --daemonize; service nginx start
