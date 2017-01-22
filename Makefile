PROJECT = csvkit
PROJECT_DESCRIPTION = csv parser with callback
PROJECT_VERSION = 0.0.1

DEPS += ecsv
dep_ecsv = git https://github.com/rcouch/ecsv master

include erlang.mk
