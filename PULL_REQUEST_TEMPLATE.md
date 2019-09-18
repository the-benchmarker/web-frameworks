Before submitting your PR, please review the following checklist :

## If you are adding a framework

+ [ ] A `Dockerfile` exists, or can be built ?
+ [ ] I've updated `CI` config file (and **commited** `.travis.yml`)
~~~sh
bin/make ci_config
~~~
+ [ ] I've updated `.dependabot` config file  (and **commited** `.dependabot/config.yml`)
~~~sh
bin/make deps_config
~~~
+ [ ] All tests passes ?
~~~
export FRAMEWORK=<MY_FRAMEWORK>
shards install
shards build --release
bin/make config
bin/neph ${FRAMEWORK}
crystal spec
~~~
