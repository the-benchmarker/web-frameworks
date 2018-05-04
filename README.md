# Benchmark

A collection of benchmarks for popular web frameworks.


## Usage

These benchmarks are powered by Docker, first make sure Docker is installed and running.

There are two separate parts to configuring this benchmark: hosting the frameworks and running the benchmarks.
These tasks can be done on separate machines, and this is recommended if you are looking for the most accurate results.

### Hosting the frameworks

Docker compose is used to host the frameworks.

```sh
docker-compose up -d
```

That command should build and boot up all of the frameworks. They all bind to an assigned port. You can check if they are running afterward by running:

```sh
docker ps
```

### Running the benchmarks

The benchmarking package is written in Swift and uses `wrk`. It is also run using Docker. Use the `Makefile` to build this image.

```sh
make witf
```

After the image is built, you can run the benchmarks.

```sh
docker run witf .build/release/benchmarker wrk 10.132.47.133
```

Use `--help` for more information.

```sh
docker run witf .build/release/benchmarker wrk --help
```