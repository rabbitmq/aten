PROJECT = aten
PROJECT_DESCRIPTION = Node failure detector
PROJECT_VERSION = 0.1.0
PROJECT_MOD = aten_app

define PROJECT_ENV
[
	{poll_interval, 1000},
	{heartbeat_interval, 100},
	{detection_threshold, 0.99}
]
endef

TEST_DEPS = proper meck eunit_formatters

LOCAL_DEPS = sasl crypto

PLT_APPS += eunit meck proper syntax_tools erts kernel stdlib common_test inets

DIALYZER_OPTS += --src -r test
EUNIT_OPTS = no_tty, {report, {eunit_progress, [colored, profile]}}

RABBITMQ_UPSTREAM_FETCH_URL ?= https://github.com/rabbitmq/aten.git

include erlang.mk

.PHONY: show-upstream-git-fetch-url

shell: app

show-upstream-git-fetch-url:
	@echo $(RABBITMQ_UPSTREAM_FETCH_URL)
