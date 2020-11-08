clean:
	find . -type f -name .Makefile -exec rm -fr {} \;
	find . -type f -name .Dockerfile -exec rm -fr {} \;
	find . -type f -name cid.txt -exec rm -fr {} \;
	find . -type f -name ip.txt -exec rm -fr {} \;
	find . -mindepth 2 -type d -name config -exec rm -fr {} \;