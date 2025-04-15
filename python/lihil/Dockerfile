FROM python:3.13-slim

WORKDIR /usr/src/app

  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -y update

    RUN apt-get -y install build-essential
    RUN apt-get -y install gcc g++


  COPY server.py server.py
  COPY pyproject.toml pyproject.toml

  RUN pip install uvicorn[standard]

  ENV PYTHON_APP=server:app

RUN pip install .

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT uvicorn \
  --log-level critical \
  --host 0.0.0.0 \
  --port 3000 \
  --workers $(nproc) \
  ${PYTHON_APP}