FROM python:3.6

WORKDIR /usr/src/app

COPY requirements.txt server.py ./

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

CMD gunicorn --bind 0.0.0.0:3000 --reuse-port --worker-class aiohttp.GunicornUVLoopWebWorker --workers $(nproc) server:app
