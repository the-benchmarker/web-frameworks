RETVAL=0
echo $PATH

if [ ${LANGUAGE} == "csharp" ] ; then
  sudo apt-get -qy install astyle
  find ${DIRECTORY} -type f -name '*.cs' > /tmp/list.txt
  while read line ; do
    astyle --mode=cs ${file}
    retval=$?
    if [ $retval -ne 0 ]; then
      RETVAL=${retval}
    fi
  done < /tmp/list.txt
  RETVAL=$?
fi

if [ ${LANGUAGE} == "elixir" ] ; then
  find ${DIRECTORY} -type f -name '*.exs' -or -name '*.ex' > /tmp/list.txt
  while read line ; do
    mix format ${file}
    retval=$?
    if [ $retval -ne 0 ]; then
      RETVAL=${retval}
    fi
  done < /tmp/list.txt
  RETVAL=$?
fi

if [ ${LANGUAGE} == "scala" ] ; then
  sudo coursier bootstrap org.scalameta:scalafmt-cli_2.12:2.0.0-RC5 \
    -r sonatype:snapshots \
    -o /usr/local/bin/scalafmt --standalone --main org.scalafmt.cli.Cli
  scalafmt ${DIRECTORY} --test
  RETVAL=$?
fi

if [ ${LANGUAGE} == "cpp" ] ; then
  sudo apt-get -qy install clang-format npm
  sudo ln -sfv /usr/bin/nodejs /usr/bin/node
  sudo npm -g install clang-format-check
  find ${DIRECTORY} -type f -name '*.cpp' > /tmp/list.txt
  while read line ; do
    clang-format-check ${file}
    retval=$?
    if [ $retval -ne 0 ]; then
      RETVAL=${retval}
    fi
  done < /tmp/list.txt
  RETVAL=$?
fi

if [ ${LANGUAGE} == "php" ] ; then
  sudo composer global require friendsofphp/php-cs-fixer
  php-cs-fixer fix php --rules=@PSR1,@PSR2
  RETVAL=$?
fi

if [ ${LANGUAGE} == "python" ] ; then
  pip install black --user
  black ${DIRECTORY} --check
  RETVAL=$?
fi

if [ ${LANGUAGE} == "c" ] ; then
  sudo apt-get -qy install clang-format npm
  sudo ln -sfv /usr/bin/nodejs /usr/bin/node
  sudo npm -g install clang-format-check
  find ${DIRECTORY} -type f -name '*.c' > /tmp/list.txt
  while read line ; do
    clang-format-check ${file}
    retval=$?
    if [ $retval -ne 0 ]; then
      RETVAL=${retval}
    fi
  done < /tmp/list.txt
  RETVAL=$?
fi

if [ ${LANGUAGE} == "nim" ] ; then
  sudo apt-get -qy install nim
  cd `mktemp -d`
  git clone https://github.com/nim-lang/Nim .
  git checkout master
  nim c nimpretty.nim  
  find ${DIRECTORY} -type f -name '*.nim' -or -name '*.nimble'  > /tmp/list.txt
  while read line ; do
    nimpretty ${file}
    retval=$?
    if [ $retval -ne 0 ]; then
      RETVAL=${retval}
    fi
  done < /tmp/list.txt
  RETVAL=$?
fi

if [ ${LANGUAGE} == "java" ] ; then
  sudo apt-get -qy install clang-format npm
  sudo ln -sfv /usr/bin/nodejs /usr/bin/node
  sudo npm -g install clang-format-check
  find ${DIRECTORY} -type f -name '*.java' > /tmp/list.txt
  while read line ; do
    clang-format-check --style=google ${file}
    retval=$?
    if [ $retval -ne 0 ]; then
      RETVAL=${retval}
    fi
  done < /tmp/list.txt
  RETVAL=$?
fi

if [ ${LANGUAGE} == "objc" ] ; then
  sudo apt-get -qy install clang-format npm
  sudo ln -sfv /usr/bin/nodejs /usr/bin/node
  sudo npm -g install clang-format-check
  find ${DIRECTORY} -type f -name '*.m' > /tmp/list.txt
  while read line ; do
    clang-format-check ${file}
    retval=$?
    if [ $retval -ne 0 ]; then
      RETVAL=${retval}
    fi
  done < /tmp/list.txt
  RETVAL=$?
fi

if [ ${LANGUAGE} == "ruby" ] ; then
  gem install rubocop
  rubocop ${DIRECTORY}
  RETVAL=$?
fi

if [ ${LANGUAGE} == "crystal" ] ; then
  crystal tool format --check ${DIRECTORY}
  RETVAL=$?
fi

if [ ${LANGUAGE} == "go" ] ; then
  go get golang.org/x/lint/golint
  golint -set_exit_status=true ${DIRECTORY}
  RETVAL=$?
fi

if [ ${LANGUAGE} == "swift" ] ; then
  swiftlint lint ${DIRECTORY}
  RETVAL=$?
fi

if [ ${LANGUAGE} == "rust" ] ; then
  rustup component add rustfmt --toolchain stable-x86_64-unknown-linux-gnu
  find ${DIRECTORY} -type f -name '*.rs' > /tmp/list.txt
  while read line ; do
    rustfmt --check ${file}
    retval=$?
    if [ $retval -ne 0 ]; then
      RETVAL=${retval}
    fi
  done < /tmp/list.txt
  RETVAL=$?
fi

if [ ${LANGUAGE} == "node" ] ; then
  npm -g install prettier
  prettier --check "node/**/*.{js,json}"
  RETVAL=$?
fi

exit ${RETVAL}
