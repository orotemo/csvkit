PROJECT = csvkit
PROJECT_DESCRIPTION = csv parser with callback
PROJECT_VERSION = 0.0.1

DEPS += ecsv file_handler
dep_ecsv = git https://github.com/rcouch/ecsv master
dep_file_handler = git https://github.com/yinbal/file_handler.git master

include erlang.mk
