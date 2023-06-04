#docker-clean && digitalocean-clean && rm -fr .neph && mkdir -p .neph/{c,clojure,cpp,crystal,csharp,dart,elixir,fsharp,go,haskell,java,javascript,julia,kotlin,nim,pony,python,ruby,rust,scala,swift} && dropdb -U postgres benchmark && createdb -U postgres benchmark && psql -U postgres benchmark < .ci/dump.sql && PROVIDER=digitalocean rake config && bin/neph --seq 

#. .env/default
#. .env/development
#
#doctl compute droplet create sieger --region ${DO_REGION} --image ${DO_IMAGE} --size ${DO_SIZE} --wait --ssh-keys ${DO_FINGERPRINT} --enable-private-networking
#sleep 30
#IP=$(doctl compute droplet list sieger --format PublicIPv4 --no-header)
#rsync -avz -e "ssh -i ${DO_KEY} -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null" --progress * root@${IP}:/root
#scp -i ${DO_KEY} -r .ci root@${IP}:/root/
#scp -i ${DO_KEY} -r .env root@${IP}:/root/.env
#scp -i ${DO_KEY} ${DO_KEY} root@${IP}:${DO_KEY}
#doctl compute ssh --ssh-key-path ${DO_KEY} sieger

# Clean database
#dropdb -U postgres benchmark
#createdb -U postgres benchmark
#psql -U postgres -d benchmark < dump.sql

find nim -mindepth 2 -type f -name config.yaml > /tmp/list.txt

while read line ; do 
  echo "*********** ${line} *************"
  LANGUAGE=`echo $line | awk -F '/' '{print $(NF-2)}'`
  FRAMEWORK=`echo $line | awk -F '/' '{print $(NF-1)}'`
 # cd ${LANGUAGE}/${FRAMEWORK}
  make -f ~/workspace/benchmark/web/${LANGUAGE}/${FRAMEWORK}/.Makefile build
#  cd ../..
  make -f ~/workspace/benchmark/web/${LANGUAGE}/${FRAMEWORK}/.Makefile collect
  make -f ~/workspace/benchmark/web/${LANGUAGE}/${FRAMEWORK}/.Makefile clean
  if [ "${FRAMEWORK}" == "axum" ] ; then
    docker ps -aq | xargs --no-run-if-empty docker rm -f;
    docker images -aq | xargs --no-run-if-empty docker rmi -f
  fi
  if [ "${FRAMEWORK}" == "siler" ] ; then
    docker ps -aq | xargs --no-run-if-empty docker rm -f;
    docker images -aq | xargs --no-run-if-empty docker rmi -f
  fi
  if [ "${FRAMEWORK}" == "orion" ] ; then
    docker ps -aq | xargs --no-run-if-empty docker rm -f;
    docker images -aq | xargs --no-run-if-empty docker rmi -f
  fi
  if [ "${FRAMEWORK}" == "merly" ] ; then
    docker ps -aq | xargs --no-run-if-empty docker rm -f;
    docker images -aq | xargs --no-run-if-empty docker rmi -f
  fi
  sleep 1
done < /tmp/list.txt

#echo 'select label from frameworks' | psql -U postgres -d benchmark -t | sort > /tmp/done.txt
#find . -mindepth 3 -type f -name config.yaml | awk -F '/' '{print $(NF-1)}' | sort > all.txt
