Before submitting your PR, please review the following checklist :

## If you are adding a framework

+ [ ] Please check configuration files
   + [ ] `neph.yml`, for [neph](http://tbrand.github.io/neph) job processing
   + [ ] `FRAMEWORKS.yml`, for **frameworks** list
   + [ ] `travis.yml`, for CI
+ [ ] A `Dockerfile` exists ?
+ [ ] All tests passes ?
~~~
export FRAMEWORK=<MY_FRAMEWORK>
shards install
shards build --release
bin/neph ${FRAMEWORK}
crystal spec
~~~
