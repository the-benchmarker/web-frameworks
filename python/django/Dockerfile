FROM python

WORKDIR /usr/src/app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 3000

CMD [ "gunicorn", "--chdir", "/usr/src/app", "--bind", "0.0.0.0:3000", "app.wsgi" ]
