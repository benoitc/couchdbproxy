% Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
% read_config, set-erlenv functions come from riak project under Apache License 2
% Copyright 2007-2009 Basho Technologies
%

-module(couchdbproxy_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).
-export([read_config/0, read_config/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for couchdbproxy.
start(_Type, _StartArgs) ->
    case couchdbproxy:get_app_env(no_config) of
        true -> nop; % promising to set all env variables some other way
        _ -> read_config()
    end,
    register(couchdbproxy_app, self()),
    erlang:set_cookie(node(), couchdbproxy:get_app_env(couchdbproxy_cookie)),
    couchdbproxy_deps:ensure(),
    couchdbproxy_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for couchdbproxy.
stop(_State) ->
    ok.

%% @spec read_config() -> ok
%% @doc Read the couchdbproxy erlenv configuration file and set environment variables.
read_config() -> read_config(couchdbproxy:get_app_env(configpath)).
%% @spec read_config(ConfigPath :: list()) -> ok
%% @doc Read the couchdbproxy erlenv configuration file with filename ConfigPath
%%      and set environment variables.
read_config(ConfigPath) ->
    ConfigPairs = 
        case file:consult(ConfigPath) of
            {ok, Terms} -> Terms;
            {error, Reason} ->
                error_logger:info_msg("Failed to read config from: ~p (~p)~n",
                                      [ConfigPath, Reason]),
                []
	end,
    set_erlenv(ConfigPairs),
    ok.

%% @private
set_erlenv([]) ->
    ok;
%% @private
set_erlenv([{K, V}|T]) when is_atom(K) ->
    application:set_env(couchdbproxy, K, V),
    error_logger:info_msg("set env variable ~p:~p~n",[K,V]),
    set_erlenv(T).