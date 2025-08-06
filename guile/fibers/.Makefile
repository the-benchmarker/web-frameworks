build:
	 cd guile/fibers && docker build -f .Dockerfile.fibers -t guile.fibers.fibers . && cd -
	 docker run -td guile.fibers.fibers > guile/fibers/cid-fibers.txt
	 docker inspect `cat guile/fibers/cid-fibers.txt` | jq -r '.[0].NetworkSettings.IPAddress' > guile/fibers/ip.txt
collect:
	 HOSTNAME=`cat guile/fibers/ip.txt` ENGINE=fibers LANGUAGE=guile FRAMEWORK=fibers DATABASE_URL=postgresql://postgres@localhost/benchmark bundle exec rake collect
clean:
	 docker ps -a -q  --filter ancestor=guile.fibers.fibers | xargs docker rm -f
run-all : build.fibers collect.fibers clean.fibers
