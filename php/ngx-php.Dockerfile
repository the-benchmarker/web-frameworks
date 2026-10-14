# syntax=docker/dockerfile:1
FROM ubuntu:26.04

ARG DEBIAN_FRONTEND=noninteractive

{{#language.version}}
  ENV PHP_VERSION={{{.}}}
{{/language.version}}
{{^language.version}}
  ENV PHP_VERSION=8.5
{{/language.version}}

RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    libzip-dev \
    curl \
    wget \
    git \
    libxml2-dev \
    systemtap-sdt-dev \
    ca-certificates \
    software-properties-common \
    zlib1g-dev libpcre2-dev libargon2-dev libsodium-dev libkrb5-dev \
    php-cli php-zip php-dev libphp-embed \
    {{#deps}}{{{.}}} {{/deps}} \
    {{#build_deps}}{{{.}}} {{/build_deps}} && \
  rm -rf /var/lib/apt/lists/*

# php.ini
COPY <<'EOT' /usr/local/etc/php/conf.d/90-php.ini
opcache.enable=1
opcache.enable_cli=1
opcache.memory_consumption=512
opcache.interned_strings_buffer=64
opcache.max_accelerated_sources=32531
opcache.validate_timestamps=0
opcache.save_comments=1
display_startup_errors = Off

opcache.jit=tracing
opcache.jit_buffer_size=128M
EOT

RUN if [ -f php.ini ] ; then \
       cp php.ini /usr/local/etc/php/conf.d/99-php.ini; fi

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer
COPY --from=ghcr.io/php/pie:bin /pie /usr/bin/pie

#default environment variables
ENV NGINX_VERSION=1.30.5
ENV NGX_PHP_VERSION=v0.0.30
{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#before_build}}
  RUN {{{.}}}
{{/before_build}}

# Build ngx-php and install
RUN git clone -b ${NGX_PHP_VERSION} --single-branch --depth 1 https://github.com/rryqszq4/ngx-php.git > /dev/null

RUN wget -q http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar -zxf nginx-${NGINX_VERSION}.tar.gz && \
    cd nginx-${NGINX_VERSION} && \
    export PHP_LIB=/usr/lib && \
    ./configure --user=www --group=www \
            --prefix=/nginx \
            --with-ld-opt="-Wl,-rpath,$PHP_LIB" \
            --add-module=/ngx-php/third_party/ngx_devel_kit \
            --add-module=/ngx-php > /dev/null && \
    make > /dev/null && make install > /dev/null

{{#extensions}}
  RUN pie install --no-cache {{{.}}}
{{/extensions}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{^framework.standalone}}
  {{#bootstrap}}
    RUN {{{.}}}
  {{/bootstrap}}
{{/framework.standalone}}

COPY <<'EOT' /usr/src/nginx.conf
user www-data;
worker_cpu_affinity auto;
worker_processes  auto;
error_log stderr error;
#worker_rlimit_nofile 1024000;
daemon off;
pcre_jit on;

events {
    worker_connections 1024;
	multi_accept off;	 
}

http {
    #include       /etc/nginx/mime.types;
    access_log off;
    server_tokens off;
    msie_padding off;

    sendfile off;
    tcp_nopush off;
    tcp_nodelay on;
    keepalive_timeout 65s;
    keepalive_disable none;
    keepalive_requests 100000;

    php_ini_path /usr/local/etc/php/conf.d/90-php.ini;

    server {
        listen       *:3000 reuseport;

        include /usr/src/root.conf;
        index  index.html;

        php_keepalive 256;

        include /usr/src/location.conf;
    }
}
EOT

#default values
COPY <<'EOT' /usr/src/root.conf
        root /;
EOT

COPY <<'EOT' /usr/src/location.conf
        location = / {
            content_by_php '
                echo "";
            ';
        }

        location = /user {
            content_by_php '
                echo "";
            ';
        }

        location /user/ {
            content_by_php '
                echo explode("/", ngx_request_uri())[2];
            ';
        }
EOT

{{#docroot}}
    COPY "root {{{.}}};"  /usr/src/root.conf
{{/docroot}}

{{#nginx_conf}}
       COPY {{{.}}} /usr/src/location.conf
{{/nginx_conf}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

{{#command}}
  ENTRYPOINT {{{.}}}
{{/command}}
{{^command}}
  ENTRYPOINT ["/nginx/sbin/nginx", "-c", "/usr/src/nginx.conf"]
{{/command}}
