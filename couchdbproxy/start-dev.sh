#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin \
         -pa $PWD/deps/*/ebin \
         -pa $PWD/deps/*/deps/*/ebin \
         -boot start_sasl \
         -s reloader \
         -s couchdbproxy
