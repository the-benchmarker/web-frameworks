FROM python:3.7.3

WORKDIR /usr/src/app

COPY requirements.txt wsgi.py ./
COPY app app
COPY bootstrap bootstrap
COPY config config
COPY databases databases
COPY resources resources
COPY routes routes

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

RUN mkdir -p storage/public
RUN mkdir -p storage/static
RUN mkdir -p storage/uploads
RUN mkdir -p storage/compiled

CMD gunicorn --log-level warning --bind 0.0.0.0:3000 --reuse-port --workers $(nproc) --worker-class meinheld.gmeinheld.MeinheldWorker wsgi
