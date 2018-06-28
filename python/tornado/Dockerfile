FROM python

WORKDIR /usr/src/app

COPY requirements.txt server.py ./

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

CMD gunicorn --bind 0.0.0.0:3000 --worker-class tornado --workers $(nproc) server:app gunicorn
