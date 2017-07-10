PROJECT = csvkit
PROJECT_DESCRIPTION = csv parser with callback
PROJECT_VERSION = 0.0.1

DEPS += file_handler
dep_file_handler = git https://github.com/yinbal/file_handler.git master

include erlang.mk
