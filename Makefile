all:
	make -C couchdbproxy

setup:
	(cd scripts; ./setup.py)

clean:
	make -C couchdbproxy clean
