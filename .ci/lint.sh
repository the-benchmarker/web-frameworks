#!/bin/bash

set -eu

if [[ ${LANGUAGE} == "csharp" ]] ; then
  sudo apt -y install astyle
  find ${DIRECTORY} -type f -name '*.cs' > /tmp/list.txt
  while read file ; do
    astyle --mode=cs ${file}
  done < /tmp/list.txt
  RETVAL=$?
fi

if [[ ${LANGUAGE} == "elixir" ]] ; then
  find ${DIRECTORY} -type f -name '*.exs' -or -name '*.ex' > /tmp/list.txt
  while read file ; do
    mix format ${file}
  done < /tmp/list.txt
  RETVAL=$?
fi

if [[ ${LANGUAGE} == "scala" ]] ; then
  curl -Lo coursier https://git.io/coursier-cli && chmod +x coursier && sudo install coursier /usr/bin
  sudo coursier bootstrap org.scalameta:scalafmt-cli_2.12:2.0.0-RC5 \
    -r sonatype:snapshots \
    -o /usr/local/bin/scalafmt --standalone --main org.scalafmt.cli.Cli
  scalafmt ${DIRECTORY} --test
fi

if [[ ${LANGUAGE} == "cpp" ]] ; then
  find ${DIRECTORY} -type f -name '*.cpp' > /tmp/list.txt
  while read file ; do
    clang-format -verbose -i ${file}
  done < /tmp/list.txt
  RETVAL=$?
fi

if [[ ${LANGUAGE} == "php" ]] ; then
  composer global require friendsofphp/php-cs-fixer
  ~/.config/composer/vendor/bin/php-cs-fixer fix --verbose php --rules=@PSR1,@PSR2 --using-cache=no
fi

if [[ ${LANGUAGE} == "python" ]] ; then
  pip install black
  black ${DIRECTORY} --check
fi

if [[ ${LANGUAGE} == "c" ]] ; then
  find ${DIRECTORY} -type f -name '*.c' > /tmp/list.txt
  while read file ; do
    clang-format -verbose -i ${file}
  done < /tmp/list.txt
fi

if [[ ${LANGUAGE} == "nim" ]] ; then
  sudo apt -y update
  sudo apt -y install nim
  cd `mktemp -d`
  git clone https://github.com/nim-lang/Nim .
  git checkout master
  cd nimpretty
  nim c nimpretty.nim
  echo "Using nimpretty version : `nimpretty -v`"
  find ${DIRECTORY} -type f -name '*.nim' -or -name '*.nimble'  > /tmp/list.txt
  while read file ; do
    nimpretty ${file}
  done < /tmp/list.txt
fi

if [[ ${LANGUAGE} == "java" ]] ; then
  find ${DIRECTORY} -type f -name '*.java' > /tmp/list.txt
  while read file ; do
    clang-format -verbose -style=google -i ${file}
  done < /tmp/list.txt
fi

if [[ ${LANGUAGE} == "objc" ]] ; then
  find ${DIRECTORY} -type f -name '*.m' > /tmp/list.txt
  while read file ; do
    clang-format -verbose -i ${file}
  done < /tmp/list.txt
fi

if [[ ${LANGUAGE} == "ruby" ]] ; then
  gem install rubocop
  rubocop -dE ${DIRECTORY}
fi

if [[ ${LANGUAGE} == "crystal" ]] ; then
  crystal tool format --check ${DIRECTORY}
fi

if [[ ${LANGUAGE} == "go" ]] ; then
  sudo apt install golint
  golint -set_exit_status=true ${DIRECTORY}
fi

if [[ ${LANGUAGE} == "swift" ]] ; then
  swiftlint lint --strict --path ${DIRECTORY}
fi

if [[ ${LANGUAGE} == "rust" ]] ; then
  rustup component add rustfmt --toolchain stable-x86_64-unknown-linux-gnu
  find ${DIRECTORY} -type f -name '*.rs' > /tmp/list.txt
  while read file ; do
    rustfmt --verbose --check ${file}
  done < /tmp/list.txt
fi

if [[ ${LANGUAGE} == "node" ]] ; then
  npm -g install prettier
  prettier --check "${DIRECTORY}/**/*.{js,json}"
fi

if [[ ${LANGUAGE} == "kotlin" ]] ; then
  curl -sS https://keybase.io/pinterestandroid/pgp_keys.asc | sudo gpg --import
  curl -sSLO https://github.com/pinterest/ktlint/releases/download/0.33.0/ktlint && chmod +x ktlint && sudo install ktlint /usr/bin
  find ${DIRECTORY} -type f -name '*.kt' > /tmp/list.txt
  while read file ; do
    ktlint --debug --verbose ${file}
  done < /tmp/list.txt
fi
