{{#language.version}}
  FROM php:{{{.}}}-fpm{{#language.dist}}-{{{.}}}{{/language.dist}}
{{/language.version}}
{{^language.version}}
  FROM php:8.5-fpm
{{/language.version}}

RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    libzip-dev \
    curl \
    libtool \
    unzip \
    {{#deps}}{{{.}}} {{/deps}} \
    {{#build_deps}}{{{.}}} {{/build_deps}} && \
  rm -rf /var/lib/apt/lists/*

#COPY ../php.ini /usr/local/etc/php/conf.d/99-php.ini;
RUN echo 'opcache.enable=1\n\
opcache.enable_cli=1\n\
opcache.memory_consumption=512\n\
opcache.interned_strings_buffer=64\n\
opcache.max_accelerated_sources=32531\n\
opcache.validate_timestamps=0\n\
opcache.save_comments=1\n\
opcache.jit=tracing\n\
opcache.jit_buffer_size=128M\n'\
  >> /usr/local/etc/php/conf.d/90-php.ini
RUN if [ -f php.ini ] ; then \
       cp php.ini /usr/local/etc/php/conf.d/99-php.ini; fi

WORKDIR /usr/src/app

RUN docker-php-ext-install zip {{#modules}}{{{.}}} {{/modules}}
COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer
COPY --from=ghcr.io/php/pie:bin /pie /usr/bin/pie

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#before_build}}
  RUN {{{.}}}
{{/before_build}}

{{#extensions}}
  RUN pie install --no-cache {{{.}}}
{{/extensions}}

ENV EVENT_EXT_FILE=/usr/local/etc/php/conf.d/docker-php-ext-event.ini
RUN if [ -f "${EVENT_EXT_FILE}" ] ; then \
  rm -fr /usr/local/etc/php/conf.d/docker-php-ext-sockets.ini ; \
  sed -i -e '1i extension=sockets.so' /usr/local/etc/php/conf.d/docker-php-ext-event.ini ; fi

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{^framework.standalone}}
  {{#bootstrap}}
    RUN {{{.}}}
  {{/bootstrap}}
{{/framework.standalone}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

{{#command}}
  ENTRYPOINT {{{.}}}
{{/command}}
{{^command}}
  # PHP-FPM configuration
  {{#slasheddocroot}}
    RUN sed -i 's/\;prefix.*/prefix = {{{.}}}/g' /usr/local/etc/php-fpm.d/www.conf
  {{/slasheddocroot}}
  {{^slasheddocroot}}
    RUN sed -i 's/\;prefix.*/prefix = \/usr\/src\/app\/public/g' /usr/local/etc/php-fpm.d/www.conf
  {{/slasheddocroot}}

  RUN sed -i 's/\;\(listen =\).*/\1 \/var\/run\/php-fpm.sock/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.owner.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.group.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  RUN sed -i 's/\;\(listen\.mode.*\).*/\1/g' /usr/local/etc/php-fpm.d/www.conf
  # pm.max_children set according to nproc
  RUN if [ $(nproc) = 1 ] ; then sed -i 's/\(pm\.max_children =\).*/\1 512/g' /usr/local/etc/php-fpm.d/www.conf ; else sed -i 's/\(pm\.max_children =\).*/\1 1024/g' /usr/local/etc/php-fpm.d/www.conf ; fi
  # after 15 seconds warm-up, half of the 'idle' children are not killed
  RUN if [ $(nproc) = 1 ] ; then sed -i 's/\(pm\.max_spare_servers =\).*/\1 256/g' /usr/local/etc/php-fpm.d/www.conf ; else sed -i 's/\(pm\.max_spare_servers =\).*/\1 512/g' /usr/local/etc/php-fpm.d/www.conf ; fi

  RUN rm -fr /usr/local/etc/php-fpm.d/zz-docker.conf

  # Nginx configuration
  COPY <<'EOT' /usr/src/nginx.conf
  user www-data;
  worker_cpu_affinity auto;
  worker_processes  auto;
  error_log stderr error;
  #worker_rlimit_nofile 2000000;
  #timer_resolution 1s;
  daemon off;

  events {
      worker_connections 1024;
      multi_accept off;
  }

  http {
      include       /etc/nginx/mime.types;
      default_type  application/octet-stream;
      access_log off;
      server_tokens off;

      sendfile on;
      tcp_nopush on;
      tcp_nodelay on;
      keepalive_timeout 65;
      keepalive_disable none;
      #fastcgi_ignore_client_abort on;
      keepalive_requests 10000000;

      #the bench not use any static file
      #open_file_cache max=2000 inactive=20s;
      #open_file_cache_valid 60s;
      #open_file_cache_min_uses 5;
      #open_file_cache_errors off;

      #FastCGI optimizations
      fastcgi_buffers 256 16k;
      fastcgi_buffer_size 128k;
      fastcgi_connect_timeout 30s;
      fastcgi_send_timeout 60s;
      fastcgi_read_timeout 60s;
      fastcgi_busy_buffers_size 256k;
      fastcgi_temp_file_write_size 256k;
      reset_timedout_connection on;
      server_names_hash_bucket_size 100;
      fastcgi_keep_conn on;
      fastcgi_socket_keepalive on;

      upstream fastcgi_backend {
          server unix:/var/run/php-fpm.sock;
          keepalive 36;
      }

      server {
          listen       3000 reuseport;
          server_name  localhost;

          include /usr/src/root.conf;
          index  index.php;

          include /usr/src/location.conf;
      }
  }
EOT

  #default values
  COPY <<'EOT' /usr/src/root.conf
        root /usr/src/app/public;
EOT

  COPY <<'EOT' /usr/src/location.conf
        location / {
          fastcgi_pass   fastcgi_backend;
          fastcgi_index  index.php;
          fastcgi_param  SCRIPT_FILENAME   $document_root/index.php;
          include fastcgi_params;
        }
EOT

  COPY <<'EOT' /usr/src/fastcgi_params
fastcgi_param  QUERY_STRING       $query_string;
fastcgi_param  REQUEST_METHOD     $request_method;
fastcgi_param  CONTENT_TYPE       $content_type;
fastcgi_param  CONTENT_LENGTH     $content_length;

fastcgi_param  SCRIPT_NAME        $fastcgi_script_name;
fastcgi_param  REQUEST_URI        $request_uri;
fastcgi_param  DOCUMENT_URI       $document_uri;
fastcgi_param  DOCUMENT_ROOT      $document_root;
fastcgi_param  SERVER_PROTOCOL    $server_protocol;
fastcgi_param  REQUEST_SCHEME     $scheme;
fastcgi_param  HTTPS              $https if_not_empty;

fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
fastcgi_param  SERVER_SOFTWARE    nginx/$nginx_version;

fastcgi_param  REMOTE_ADDR        $remote_addr;
fastcgi_param  REMOTE_PORT        $remote_port;
fastcgi_param  SERVER_ADDR        $server_addr;
fastcgi_param  SERVER_PORT        $server_port;
fastcgi_param  SERVER_NAME        $server_name;


# PHP only, required if PHP was built with --enable-force-cgi-redirect
fastcgi_param  REDIRECT_STATUS    200;
EOT

  {{#docroot}}
    COPY "root {{{.}}};" /usr/src/root.conf
  {{/docroot}}
  
  {{#nginx_conf}}
       COPY {{{.}}} /usr/src/location.conf
  {{/nginx_conf}}

  ENTRYPOINT /usr/local/sbin/php-fpm --daemonize; nginx -c /usr/src/nginx.conf
{{/command}}
