FROM php:7.4-fpm-alpine

RUN apk add git zlib-dev libzip-dev build-base autoconf nginx openrc curl-dev icu-dev oniguruma-dev 
RUN docker-php-ext-install zip opcache

WORKDIR /usr/src/app

  RUN pecl install psr
  RUN docker-php-ext-enable psr
  RUN pecl install phalcon
  RUN docker-php-ext-enable phalcon




COPY . ./

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o




    RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.owner.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.group.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.mode.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  
  RUN rm -fr /etc/nginx/sites-enabled/default
  RUN rm -fr /usr/local/etc/php-fpm.d/zz-docker.conf
  RUN mkdir /run/nginx
  
  RUN echo -e 'server {\n\
        root /usr/src/app/public;\n\
      listen 0.0.0.0:3000;\n\
        location /{\n\
    rewrite ^/(.*)$ /index.php?_url=$1 last;\n\
}\n\

        location = /index.php {\n\
  fastcgi_pass unix:/var/run/php-fpm.sock;\n\
  fastcgi_param   SCRIPT_FILENAME         $document_root/index.php;\n\
  include fastcgi_params;\n\
}\n\

  }\n'\
  >> /etc/nginx/conf.d/www.conf
  
  RUN echo -e 'opcache.enable=1\n\
  opcache.preload="/usr/src/app/preload.php"\n\
  opcache.memory_consumption=2048\n\
  opcache.interned_strings_buffer=512\n\
  opcache.max_accelerated_files=32531\n\
  opcache.validate_timestamps=1\n\
  opcache.save_comments=1\n\
  opcache.fast_shutdown=0\n'\
  >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
  
  CMD /usr/local/sbin/php-fpm --daemonize; nginx -g "daemon off;"

