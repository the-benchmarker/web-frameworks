# Sharaf

Uses Sharaf–Potateh 0.23.0, Scala 3.10.0, sbt 2.1.0 and Java 27, with a virtual-thread-per-task executor and a connection limit of 1024.

From `scala/sharaf`, build the distribution with `sbt "Universal / packageZipTarball"`.

From the repository root:

```sh
bundle install
bundle exec rake config
make -f scala/sharaf/.Makefile build
make -f scala/sharaf/.Makefile test
make -f scala/sharaf/.Makefile warmup
mkdir -p scala/sharaf/.results/10
make -f scala/sharaf/.Makefile collect
make -f scala/sharaf/.Makefile unbuild
```

Always run `unbuild` after an interrupted or failed run too. See the repository
README for CPU isolation, concurrency and route selection when generating the
Makefile. Rebuild after updating dependency versions in `build.sbt`.
