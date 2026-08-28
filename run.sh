BASEDIR=`pwd`

# zrk is the load generator every `collect` target shells out to, and nothing
# in this repo installs it. Without this check a missing zrk surfaces as a
# "command not found" per framework, hours into a run, with empty .results and
# a `make` that kept going -- so fail here instead, before the first build.
if ! command -v zrk > /dev/null 2>&1; then
	echo "zrk not found on PATH." >&2
	echo "" >&2
	echo "Install it with one of:" >&2
	echo "  brew install zoxy-io/tap/zrk" >&2
	echo "  https://github.com/zoxy-io/zrk/releases (static binaries)" >&2
	echo "" >&2
	echo "--closed (used by the collect targets) needs zrk >= 2.2.0." >&2
	exit 1
fi

if [ "$#" -eq 0 ]; then
	find . -mindepth 3 -type f -name config.yaml | grep -Ev 'imi-swoole|guildenstern' > ~/list.txt
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
