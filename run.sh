BASEDIR=`pwd`

# Host-networked benchmark servers share port 3000, including wildcard listeners
# that accept connections to 127.0.0.1. Clear it before starting each framework.
release_benchmark_port() {
  listeners=$(ss -H -ltn 'sport = :3000') || return 1
  [ -z "$listeners" ] && return 0

  echo "Releasing TCP port 3000 for 127.0.0.1"
  fuser -k -n tcp 3000 || return 1

  for attempt in 1 2 3 4 5; do
    listeners=$(ss -H -ltn 'sport = :3000') || return 1
    [ -z "$listeners" ] && return 0
    sleep 1
  done

  echo "TCP port 3000 is still in use." >&2
  return 1
}

# zrk is the load generator every `collect` target shells out to, and nothing
# in this repo installs it. Without this check a missing zrk surfaces as a
# "command not found" per framework, hours into a run, with empty .results and
# a `make` that kept going -- so fail here instead, before the first build.
#
# The version is checked too, not just the presence. A too-old zrk does not
# fail loudly: it rejects the unknown flag per invocation, which is the same
# "empty .results, make kept going" outcome the presence check exists to
# prevent -- and the check used to claim a minimum it never enforced.
ZRK_MIN=2.4.0  # --closed needs 2.2.0; 2.4.x is what the harness was validated on and
               # carries the 2.2.2 fix that halved the generator's per-request overhead

zrk_install_help() {
	echo "" >&2
	echo "Install it with one of:" >&2
	echo "  brew install zoxy-io/tap/zrk" >&2
	echo "  https://github.com/zoxy-io/zrk/releases (static binaries)" >&2
}

if ! command -v zrk > /dev/null 2>&1; then
	echo "zrk not found on PATH." >&2
	zrk_install_help
	echo "" >&2
	echo "The collect targets need zrk >= ${ZRK_MIN}." >&2
	exit 1
fi

ZRK_VERSION=`zrk --version 2>/dev/null | awk '{print $2}'`
if [ -z "$ZRK_VERSION" ]; then
	echo "could not read a version from \`zrk --version\`." >&2
	echo "Is the zrk on PATH really zoxy-io/zrk?" >&2
	exit 1
fi

# sort -V puts the lower version first, so the minimum leading means it is met
# (equal versions sort either way and both satisfy it).
if [ "`printf '%s\n%s\n' "$ZRK_MIN" "$ZRK_VERSION" | sort -V | head -n1`" != "$ZRK_MIN" ]; then
	echo "zrk ${ZRK_VERSION} is too old; the collect targets need >= ${ZRK_MIN}." >&2
	echo "(The collect targets run --closed; the harness is validated on 2.4.x.)" >&2
	zrk_install_help
	exit 1
fi

for tool in ss fuser; do
  if ! command -v "$tool" > /dev/null 2>&1; then
    echo "$tool is required to release TCP port 3000 before each benchmark." >&2
    exit 1
  fi
done

if [ "$#" -eq 0 ]; then
	find . -mindepth 3 -type f -name config.yaml | grep -Ev 'imi-swoole' > ~/list.txt
else
	COUNT=`echo $1 | grep -c "/"`
	if [ "$COUNT" -eq 0 ]; then
		find $1 -mindepth 2 -type f -name config.yaml > ~/list.txt
	elif [ "$COUNT" -eq 1 ] ; then
		find $1 -mindepth 1 -type f -name config.yaml > ~/list.txt
	else
		echo "Illegal number of parameters" >&2
		exit 2
	fi
fi

while read line ; do 
  echo "*********** ${line} *************"
  LANGUAGE=`echo $line | awk -F '/' '{print $(NF-2)}'`
  FRAMEWORK=`echo $line | awk -F '/' '{print $(NF-1)}'`
  if ! release_benchmark_port; then
    echo "Cannot release TCP port 3000; check permissions to terminate its owner." >&2
    exit 1
  fi
  rm -fr  ${BASEDIR}/${LANGUAGE}/${FRAMEWORK}/.results
  mkdir -p ${BASEDIR}/${LANGUAGE}/${FRAMEWORK}/.results/{64,256,512}
  make -f ${BASEDIR}/${LANGUAGE}/${FRAMEWORK}/.Makefile build
  sleep 60
  make -f ${BASEDIR}/${LANGUAGE}/${FRAMEWORK}/.Makefile test
  ret=$?
  if [ $ret -eq 0 ]; then
    make -f ${BASEDIR}/${LANGUAGE}/${FRAMEWORK}/.Makefile warmup
#    make -f ${BASEDIR}/${LANGUAGE}/${FRAMEWORK}/.Makefile memory-idle
    make -f ${BASEDIR}/${LANGUAGE}/${FRAMEWORK}/.Makefile collect
    sleep 5
  else
    echo "Failure in ${LANGUAGE}/${FRAMEWORK}"
  fi
  make -f ${BASEDIR}/${LANGUAGE}/${FRAMEWORK}/.Makefile unbuild
done < ~/list.txt
