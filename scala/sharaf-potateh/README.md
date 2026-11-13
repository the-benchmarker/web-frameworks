# Sharaf with Potateh on Scala Native

Uses Sharaf 0.21.0, Potateh 0.3.0, Scala 3.9.0 and Scala Native 0.5.12.
The executable is linked in `release-full` mode and uses Potateh's default
cached executor with a connection limit of 1024. The runtime image has no JVM.

The Sharaf Potateh adapter and its dependencies resolve from Maven Central.

From the repository root:

```sh
bundle install
bundle exec rake config
make -f scala/sharaf-potateh/.Makefile build
make -f scala/sharaf-potateh/.Makefile test
make -f scala/sharaf-potateh/.Makefile warmup
mkdir -p scala/sharaf-potateh/.results/10
make -f scala/sharaf-potateh/.Makefile collect
make -f scala/sharaf-potateh/.Makefile unbuild
```

Always run `unbuild` after an interrupted or failed run too. See the repository
README for CPU isolation, concurrency and route selection when generating the
Makefile. Rebuild after updating the pinned dependency versions in `deder.pkl`.
