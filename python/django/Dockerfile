FROM python

WORKDIR /usr/src/app

COPY requirements.txt settings.py ./
COPY app app

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

CMD gunicorn --chdir /usr/src/app/app --worker-class meinheld.gmeinheld.MeinheldWorker --bind 0.0.0.0:3000 --workers $(nproc) app.wsgi

