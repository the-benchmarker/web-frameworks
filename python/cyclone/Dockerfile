FROM python:3.7

WORKDIR /usr/src/app

COPY requirements.txt server.py ./

RUN pip install --no-cache-dir -r requirements.txt

CMD twistd --python=server.py --nodaemon
