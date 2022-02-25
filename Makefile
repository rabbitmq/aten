PROJECT = aten
PROJECT_DESCRIPTION = A New Adaptive Accrual Failure Detector
PROJECT_VERSION = 0.6.0

define PROJECT_ENV
[
]
endef

TEST_DEPS = proper meck eunit_formatters

LOCAL_DEPS = sasl crypto

PLT_APPS += eunit meck proper syntax_tools erts kernel stdlib common_test inets

DIALYZER_OPTS += --src -r test
EUNIT_OPTS = no_tty, {report, {eunit_progress, [colored, profile]}}

include erlang.mk

shell: app
