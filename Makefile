PROJECT = gceventer

# --------------------------------------------------------------------
# Dependencies.
# --------------------------------------------------------------------

# if we part of deps directory, we using $(CURDIR)../ as DEPS_DIR
ifeq ($(shell basename $(shell dirname $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))), deps)
    DEPS_DIR ?= $(shell dirname $(CURDIR))
endif

dep_teaser = git https://github.com/spylik/teaser develop

SHELL_DEPS = sync lager teaser

ifeq ($(USER),travis)
    TEST_DEPS += covertool
    dep_covertool = git https://github.com/idubrov/covertool
endif

# --------------------------------------------------------------------
# Development enviroment ("make shell" to run it).
# --------------------------------------------------------------------

SHELL_OPTS = -pa ebin/ test/ -env ERL_LIBS deps -eval 'lager:start(), mlibs:discover()' -run mlibs autotest_on_compile

# --------------------------------------------------------------------
# We using erlang.mk 
# --------------------------------------------------------------------

include erlang.mk
