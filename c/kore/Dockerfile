FROM ubuntu:18.04

RUN apt-get update -qq && \
    apt-get install -qq -y \
            wget \
            make \
            gcc \
            nginx \
            openssl \
            libssl-dev && \
    apt-get clean -qq -y && \
    apt-get autoclean -qq -y && \
    apt-get autoremove -qq -y

RUN wget -c https://kore.io/releases/kore-3.1.0.tar.gz && tar xvf kore-3.1.0.tar.gz
RUN cd kore-3.1.0 && TASKS=1 NOTLS=1 make && make install

WORKDIR /usr/src/app

COPY hello hello

WORKDIR /usr/src/app/hello

RUN pwd

RUN kodev build

RUN rm -fr /etc/nginx/sites-enabled/default

RUN echo 'upstream backend {\n\
    server 127.0.0.1:8888;\n\
}\n\
server {\n\
    root /usr/src/app;\n\
    listen 0.0.0.0:3000;\n\
    rewrite ^/user/(.*)$ /user_details?id=$1 last;\n\
    location / {\n\
        proxy_pass http://backend;\n\
        proxy_http_version 1.1;\n\
        proxy_set_header Connection "";\n\
    }\n\
}\n'\
>> /etc/nginx/conf.d/www.conf

RUN echo "daemon off;" >> /etc/nginx/nginx.conf

EXPOSE 3000

CMD kore -n -r -c conf/hello.conf; /etc/init.d/nginx start
