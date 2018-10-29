FROM python:3.7.1

WORKDIR /usr/src/app

COPY requirements.txt server.py ./

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

CMD gunicorn --bind 0.0.0.0:3000 --reuse-port --worker-class uvicorn.workers.UvicornWorker --workers $(nproc) server:app
