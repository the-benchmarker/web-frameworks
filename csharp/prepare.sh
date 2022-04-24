#!/bin/sh

pushd $PWD
root=$1
cd $root
echo "cd $root"

cp Dockerfile aspnet-minimal-api/
cp Dockerfile aspnet-mvc/
cp Dockerfile aspnet-mw/
cp Dockerfile beetlex/
cp Dockerfile carter/
cp Dockerfile fastendpoints/
cp Dockerfile simplify.web/

ls -alr **/Dockerfile

for d in */; do
  direc="$1/$d"
  cd $direc
  trimmed=('csharp.'`echo $d | sed 's/\/$//g'`);
  echo "docker build --no-cache -t \"$trimmed\" ."
  docker build --no-cache -t "$trimmed" .
done;

popd;