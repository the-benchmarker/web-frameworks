FROM python:3.7.1

WORKDIR /usr/src/app

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt
RUN pip install --no-cache-dir --no-binary :all: falcon==1.4.1

COPY requirements.txt server.py ./

EXPOSE 3000

CMD gunicorn --log-level warning --bind 0.0.0.0:3000 --reuse-port --worker-class meinheld.gmeinheld.MeinheldWorker --workers $(nproc) server:app
