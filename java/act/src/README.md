# WITF - ActFramework implementation

This is the implementation of [Which-Is-The-Fastest benchmark](https://github.com/tbrand/which_is_the_fastest) using [ActFramework](https://github.com/actframework/actframework)

## Requirement

You need a Java runtime (1.7+) and a Maven (3.5+) to build and run this application. Make sure you get `Java` and `mvn` added into your `PATH` environment variable.

## Run

To run the app in PROD mode (you should do this for benchmark), type `./run_prod`

## Test

To run end to end test, type `./run_e2e`

## Build

To build the dist package, type `mvn clean package`, then you should be able to find the `tar.gz` package in the `target/dist` dir. To run this binary package, just unpackage and you will find the `run` scripts in place.

