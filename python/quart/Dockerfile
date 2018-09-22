FROM python:3.6

WORKDIR /usr/src/app

COPY requirements.txt server.py ./

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

CMD uvicorn --host 0.0.0.0 --port 3000 --no-access-log server:app
