#!/bin/sh
. couchdbproxy-env.sh
cd `dirname $0`
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), couchdbproxy_app:read_config(\"$1\"), io:format(\"~p~n\",[couchdbproxy:get_app_env(couchdbproxy_name)])" -run init stop)
RHOSTNAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), couchdbproxy_app:read_config(\"$1\"), io:format(\"~s~n\",[couchdbproxy:get_app_env(couchdbproxy_hostname)])" -run init stop)
if [ "$NODENAME" = "no_couchdbproxy_nodename_undefined" ]; then
    echo "couchdbbot_nodename not set in config file, cannot start";
else
    exec erl -connect_all false -pa $PWD/deps/*/ebin -pa $PWD/ebin -name ${NODENAME}@${RHOSTNAME} -s reloader -run couchdbproxy start $1
fi