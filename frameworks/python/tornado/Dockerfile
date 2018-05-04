FROM python

WORKDIR /usr/src/app

COPY requirements.txt server.py ./

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 3000

CMD python server.py
