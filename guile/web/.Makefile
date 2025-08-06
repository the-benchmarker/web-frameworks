build:
	 cd guile/web && docker build -f .Dockerfile.web -t guile.web.web . && cd -
	 docker run -td guile.web.web > guile/web/cid-web.txt
	 docker inspect `cat guile/web/cid-web.txt` | jq -r '.[0].NetworkSettings.IPAddress' > guile/web/ip.txt
collect:
	 HOSTNAME=`cat guile/web/ip.txt` ENGINE=web LANGUAGE=guile FRAMEWORK=web DATABASE_URL=postgresql://postgres@localhost/benchmark bundle exec rake collect
clean:
	 docker ps -a -q  --filter ancestor=guile.web.web | xargs docker rm -f
run-all : build.web collect.web clean.web
