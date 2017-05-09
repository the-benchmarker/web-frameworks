# See LICENSE for licensing information.

PROJECT = cowlib
PLT_APPS = crypto
CI_OTP = OTP_R15B OTP_R15B01 OTP_R15B02 OTP_R15B03-1 OTP_R16B OTP_R16B01 OTP_R16B02 OTP_R16B03-1 OTP-17.0.2 OTP-17.1.2 OTP-17.2.2 OTP-17.3.4 OTP-17.4.1 OTP-17.5.6.3 OTP-18.0.3

include erlang.mk

TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}' -DEXTRA=1

.PHONY: gen perfs

# Mimetypes module generator.

GEN_URL = http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types
GEN_SRC = src/cow_mimetypes.erl.src
GEN_OUT = src/cow_mimetypes.erl

gen:
	$(gen_verbose) cat $(GEN_SRC) \
		| head -n `grep -n "%% GENERATED" $(GEN_SRC) | cut -d : -f 1` \
		> $(GEN_OUT)
	$(gen_verbose) wget -qO - $(GEN_URL) \
		| grep -v ^# \
		| awk '{for (i=2; i<=NF; i++) if ($$i != "") { \
			split($$1, a, "/"); \
			print "all_ext(<<\"" $$i "\">>) -> {<<\"" \
				a[1] "\">>, <<\"" a[2] "\">>, []};"}}' \
		| sort \
		| uniq -w 25 \
		>> $(GEN_OUT)
	$(gen_verbose) cat $(GEN_SRC) \
		| tail -n +`grep -n "%% GENERATED" $(GEN_SRC) | cut -d : -f 1` \
		>> $(GEN_OUT)

# Performance testing.

deps/horse:
	git clone -n -- https://github.com/extend/horse $(DEPS_DIR)/horse
	cd $(DEPS_DIR)/horse ; git checkout -q master
	$(MAKE) -C $(DEPS_DIR)/horse

perfs: ERLC_OPTS += -DPERF=1 +'{parse_transform, horse_autoexport}' -DEXTRA=1
perfs: clean deps deps/horse app
	$(gen_verbose) erl -noshell -pa ebin deps/horse/ebin \
		-eval 'horse:app_perf($(PROJECT)), init:stop().'
