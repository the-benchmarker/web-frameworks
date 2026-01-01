# Vanilla

Vanilla is a raw V server.

## Description

This project is a simple server written in the V programming language. It aims
to provide a minimalistic and efficient server implementation.

## Features

- Lightweight and fast
- Minimal dependencies
- Easy to understand and extend

## Installation

To install Vanilla, you need to have the V compiler installed. You can download
it from the [official V website](https://vlang.io).

## Usage

To run the server, use the following command:

```sh
v -prod run .
```

This will start the server, and you can access it at `http://localhost:3000`.

## Test

### CURL

```sh
curl -X GET --verbose http://localhost:3000/ &&
curl -X POST --verbose http://localhost:3000/user &&
curl -X GET --verbose http://localhost:3000/user/1
```

### WRK

```sh
wrk --connection 512 --threads 16 --duration 10s http://localhost:3000
```
