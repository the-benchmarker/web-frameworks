FROM python

WORKDIR /usr/src/app

COPY requirements.txt server.py ./

RUN pip install --no-cache-dir -r requirements.txt
RUN git clone https://github.com/ygl-rg/cyclone3.git 
RUN python setup.py install

EXPOSE 3000

CMD python server.py
