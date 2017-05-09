# See LICENSE for licensing information.

PROJECT = ranch
PROJECT_DESCRIPTION = Socket acceptor pool for TCP protocols.
PROJECT_VERSION = 1.3.2
PROJECT_REGISTERED = ranch_server

# Options.

COMPILE_FIRST = ranch_transport
CT_OPTS += -pa test -ct_hooks ranch_ct_hook []
PLT_APPS = crypto public_key ssl

CI_OTP ?= \
	OTP_R16B OTP_R16B01 OTP_R16B02 OTP_R16B03-1 \
	OTP-17.1.2 OTP-17.2.2 OTP-17.3.4 OTP-17.4.1 OTP-17.5.6.6 \
	OTP-18.0.3 OTP-18.1.5 OTP-18.2.4.1 OTP-18.3.4.4 \
	OTP-19.0.7 OTP-19.1.6
#CI_HIPE ?= $(lastword $(CI_OTP))
#CI_ERLLVM ?= $(CI_HIPE)

# Dependencies.

LOCAL_DEPS = ssl

DOC_DEPS = asciideck

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

# Standard targets.

include erlang.mk

# Also dialyze the tests.

DIALYZER_OPTS += --src -r test

# Use erl_make_certs from the tested release.

ci-setup:: $(DEPS_DIR)/ct_helper
	cp ~/.kerl/builds/$(CI_OTP_RELEASE)/otp_src_git/lib/ssl/test/erl_make_certs.erl deps/ct_helper/src/
