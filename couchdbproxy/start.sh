#!/bin/sh
. couchdbproxy-env.sh
cd `dirname $0`
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), couchdbproxy_app:read_config(\"$1\"), io:format(\"~p~n\",[couchdbproxy:get_app_env(couchdbproxy_name)])" -run init stop)
RHOSTNAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), couchdbproxy_app:read_config(\"$1\"), io:format(\"~s~n\",[couchdbproxy:get_app_env(couchdbproxy_hostname)])" -run init stop)
export HEART_COMMAND=$(erl -noshell -pa ebin -eval "error_logger:tty(false), couchdbbot_app:read_config(\"$1\"), io:format(\"~s~n\",[couchdbproxy:get_app_env(couchdbproxy_heart_command)])" -run init stop)
if [ "$NODENAME" = "no_couchdbproxy_nodename_undefined" ]; then
    echo "couchdbbot_nodename not set in config file, cannot start";
else
    
exec erl -heart -detached -connect_all false -pa $PWD/deps/*/ebin -pa $PWD/ebin -boot start_sasl true -name ${NODENAME}@${RHOSTNAME} -run couchdbbot start $1
