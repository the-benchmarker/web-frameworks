Before submitting your PR, please review the following checklist:

## If you are adding a framework

+ [ ] The framework is listed
   + [ ] On [neph](http://tbrand.github.io/neph)
   + [ ] On `FRAMEWORKS.yml`
   + [ ] On `travis.yml`
+ [ ] A `Dockerfile` exists ?
+ [ ] All tests passes ?
~~~
export FRAMEWORK=<MY_FRAMEWORK>
shards install
shards build --release
bin/neph ${FRAMEWORK}
crystal spec
~~~
