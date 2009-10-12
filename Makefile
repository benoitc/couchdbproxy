all: compile

compile:
	@mkdir -p couchdbproxy/ebin
	@mkdir -p couchdbproxy/deps/mochiweb/ebin
	@mkdir -p couchdbproxy/deps/couchbeam/ebin
	@mkdir -p couchdbproxy/deps/couchbeam/deps/lhttpc/ebin
	(cd couchdbproxy/deps/mochiweb;$(MAKE))
	(cd couchdbproxy/deps/lhttpc;$(MAKE))
	(cd couchdbproxy/deps/couchbeam;$(MAKE))
	make -C couchdbproxy

setup:
	(cd scripts; ./setup.py)

clean:
	@rm -rf	couchdbproxy/ebin/*.*
	@rm -rf couchdbproxy/deps/couchbeam/ebin/*
	@rm -rf couchdbproxy/deps/lhttpc/ebin/*
	@rm -rf couchdbproxy/deps/mochiweb/ebin/*
