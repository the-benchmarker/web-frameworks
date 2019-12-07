FROM php:7.3-fpm-alpine

RUN apk add build-base git zlib-dev nginx php7-dev libzip-dev curl {{#deps}} {{{.}}} {{/deps}}
RUN docker-php-ext-install zip opcache {{#php_mod}} {{{.}}} {{/php_mod}}

WORKDIR /usr/src/app

{{#php_mod}}
  RUN docker-php-ext-install {{{.}}}
{{/php_mod}}

{{#php_ext}}
  RUN pecl install {{{.}}}
  RUN docker-php-ext-enable {{{.}}}
{{/php_ext}}

COPY . ./

{{^standalone}}
RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --prefer-dist --classmap-authoritative
RUN composer dumpautoload -o
{{/standalone}}

{{#environment}}
ENV {{{.}}}
{{/environment}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#command}}
  CMD {{{.}}}
{{/command}}

{{^command}}
  RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.owner.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.group.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.mode.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  
  RUN rm -fr /etc/nginx/sites-enabled/default
  RUN rm -fr /usr/local/etc/php-fpm.d/zz-docker.conf
  RUN rm -fr /etc/nginx/conf.d/default

  RUN mkdir /run/nginx
  
  RUN echo 'server {\
      root /usr/src/app/public;\
      listen 0.0.0.0:3000;\
      location / {\
          fastcgi_pass unix:/var/run/php-fpm.sock;\
          fastcgi_param   SCRIPT_FILENAME         $document_root/index.php;\
          try_files $uri $uri/ /index.php;\
          include fastcgi_params;\
      }\
  }'\
  >> /etc/nginx/conf.d/www.conf
  
  RUN echo 'opcache.enable=1\
  opcache.memory_consumption=512\
  opcache.interned_strings_buffer=64\
  opcache.max_accelerated_files=32531\
  opcache.validate_timestamps=0\
  opcache.save_comments=1\
  opcache.fast_shutdown=0'\
  >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
  
  RUN echo "daemon off;" >> /etc/nginx/nginx.conf
  
  CMD /usr/local/sbin/php-fpm --daemonize; /usr/sbin/nginx
{{/command}}
