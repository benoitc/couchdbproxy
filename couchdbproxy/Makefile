all: compile

compile:
	@mkdir -p doc
	@mkdir -p ebin
	@mkdir -p deps/mochiweb/ebin
	@mkdir -p deps/couchbeam/ebin
	@mkdir -p deps/lhttpc/ebin
	(cd deps/mochiweb;$(MAKE))
	(cd deps/lhttpc;$(MAKE))
	(cd deps/couchbeam;$(MAKE))
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	@rm -rf deps/couchbeam/ebin/*
	@rm -rf deps/lhttpc/ebin/*
	@rm -rf deps/mochiweb/ebin/*
