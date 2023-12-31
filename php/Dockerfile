{{#language.version}}
  FROM php:{{{.}}}-fpm AS base
{{/language.version}}
{{^language.version}}
  FROM php:8.3-fpm AS base
{{/language.version}}

RUN apt-get -qq update
RUN apt-get -qy install libzip-dev {{#deps}} {{{.}}} {{/deps}} 
RUN docker-php-ext-install zip opcache



{{#modules}}
  FROM base AS {{{.}}}
  RUN docker-php-ext-install {{{.}}}
{{/modules}}

{{#extensions}}
  FROM base AS {{{.}}}
  RUN mkdir -p /usr/src/php/ext/{{{.}}}
  RUN curl -fsSL https://pecl.php.net/get/{{{.}}} | tar xvz -C "/usr/src/php/ext/{{{.}}}" --strip 1
  RUN docker-php-ext-install {{{.}}}
{{/extensions}}



FROM base 

WORKDIR /usr/src/app

{{#modules}}
  COPY --from={{{.}}} /usr/local/etc/php/conf.d/docker-php-ext-{{{.}}}.ini /usr/local/etc/php/conf.d/docker-php-ext-{{{.}}}.ini
  COPY --from={{{.}}} /usr/local/lib/php/extensions/no-debug-non-zts-20230831/{{{.}}}.so /usr/local/lib/php/extensions/no-debug-non-zts-20230831/{{{.}}}.so
{{/modules}}
{{#extensions}}
  COPY --from={{{.}}} /usr/local/etc/php/conf.d/docker-php-ext-{{{.}}}.ini /usr/local/etc/php/conf.d/docker-php-ext-{{{.}}}.ini
  COPY --from={{{.}}} /usr/local/lib/php/extensions/no-debug-non-zts-20230831/{{{.}}}.so /usr/local/lib/php/extensions/no-debug-non-zts-20230831/{{{.}}}.so
{{/extensions}}





{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#build_deps}}
  RUN apt-get -qy install {{{.}}}
{{/build_deps}}




ENV EVENT_EXT_FILE /usr/local/etc/php/conf.d/docker-php-ext-event.ini
RUN if [ -f "${EVENT_EXT_FILE}" ] ; then \
  rm -fr /usr/local/etc/php/conf.d/docker-php-ext-sockets.ini ; \
  sed -i -e '1i extension=sockets.so' /usr/local/etc/php/conf.d/docker-php-ext-event.ini ; fi
  
{{#before_build}}
  RUN {{{.}}}
{{/before_build}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}


{{^framework.standalone}}
  {{#bootstrap}}
    RUN {{{.}}}
  {{/bootstrap}}
{{/framework.standalone}}

{{#command}}
  CMD {{{.}}}
{{/command}}

{{^command}}
  {{#slasheddocroot}}
    RUN sed -i 's/\;prefix.*/prefix = {{{.}}}/g' /usr/local/etc/php-fpm.d/www.conf
  {{/slasheddocroot}}
  {{^slasheddocroot}}
    RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf
  {{/slasheddocroot}}
  RUN sed -i 's/\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.owner.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.group.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.mode.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf\
  RUN if [ $(nproc) = 1 ] ; then sed -i 's/\(pm\.max_children =\).*/\1 512/g' /usr/local/etc/php-fpm.d/www.conf ; else sed -i 's/\(pm\.max_children =\).*/\1 1024/g' /usr/local/etc/php-fpm.d/www.conf ; fi 
  RUN if [ $(nproc) = 1 ] ; then sed -i 's/\(pm\.max_spare_servers =\).*/\1 256/g' /usr/local/etc/php-fpm.d/www.conf ; else sed -i 's/\(pm\.max_spare_servers =\).*/\1 512/g' /usr/local/etc/php-fpm.d/www.conf ; fi 
  
  RUN rm -fr /usr/local/etc/php-fpm.d/zz-docker.conf
  
  RUN echo 'server {\n\
      {{#docroot}}
        root {{{.}}};\n\
      {{/docroot}}
      {{^docroot}}
        root /usr/src/app/public;\n\
      {{/docroot}}
      listen 0.0.0.0:3000;\n\
      {{#nginx_conf}}
        {{{.}}}
      {{/nginx_conf}}
      {{^nginx_conf}}
        location / {\n\
          try_files $uri $uri/ /index.php;\n\
          fastcgi_pass unix:/var/run/php-fpm.sock;\n\
          fastcgi_param   SCRIPT_FILENAME         $document_root/index.php;\n\
          include fastcgi_params;\n\
        }\n\
      {{/nginx_conf}}
  }\n'\
  > /etc/nginx/conf.d/server.conf
  
  RUN echo 'opcache.enable=1\n\
  opcache.memory_consumption=512\n\
  opcache.interned_strings_buffer=64\n\
  opcache.max_accelerated_sources=32531\n\
  opcache.validate_timestamps=0\n\
  opcache.save_comments=1\n\
  opcache.fast_shutdown=0\n'\
  >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
  
  CMD /usr/local/sbin/php-fpm --daemonize; nginx -g "daemon off;"

{{/command}}

