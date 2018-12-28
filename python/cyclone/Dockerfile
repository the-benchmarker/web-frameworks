FROM python:3.7.3

WORKDIR /usr/src/app

COPY requirements.txt server.py ./
COPY src src

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

CMD twistd --python server.py --nodaemon
