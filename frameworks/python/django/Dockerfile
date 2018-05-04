FROM python

WORKDIR /usr/src/app

COPY requirements.txt settings.py ./
COPY app app

RUN pip install --no-cache-dir -r requirements.txt

CMD [ "gunicorn", "--chdir", "/usr/src/app", "--bind", "0.0.0.0:8007", "app.wsgi" ]
