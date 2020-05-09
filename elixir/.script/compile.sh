
#!/bin/sh

if [ ! -d "/opt/web/release" ] ; then

    export MIX_ENV="prod"
    export ERL_COMPILER_OPTIONS="[native, {hipe, [verbose, o3]}]"
    cd /opt/web
    mix local.rebar --force | tee -a /tmp/log
    mix local.hex --force | tee -a /tmp/log
    mix deps.get --only prod | tee -a /tmp/log
    mix compile | tee -a /tmp/log
    mix release --path release | tee -a /tmp/log
fi