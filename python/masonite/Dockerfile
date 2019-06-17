FROM python:3.7.3

WORKDIR /usr/src/app

COPY requirements.txt wsgi.py ./
COPY app app
COPY bootstrap bootstrap
COPY config config
COPY routes routes

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

CMD gunicorn --log-level warning --bind 0.0.0.0:3000 --reuse-port --workers $(nproc) --worker-class meinheld.gmeinheld.MeinheldWorker wsgi
