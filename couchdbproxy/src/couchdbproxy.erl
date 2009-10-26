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
% get_app_env function come from riak project under Apache License 2
% Copyright 2007-2009 Basho Technologies

%% @doc TEMPLATE.

-module(couchdbproxy).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-export([start/0, start/1, stop/0]).
-export([get_app_env/2,get_app_env/1]).


%% @spec start([ConfigPath :: list()]) -> ok
%% @doc Start couchdbbot.
%%      ConfigPath specifies the location of the couchdbbot configuration file.
start([ConfigPath]) ->
    application:set_env(couchdbproxy, configpath, ConfigPath),
    start().

        
%% @spec start() -> ok
%% @doc Start the couchdbproxy server.
start() ->
    couchdbproxy_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(ssl),
    ensure_started(lhttpc),
    ensure_started(couchbeam),
    application:start(couchdbproxy).


%% @spec stop() -> ok
%% @doc Stop the couchdbproxy application and the calling process.
stop() -> stop("couchdbproxy stop requested").

%% @spec stop(Reason) -> ok
%% @doc Stop the couchdbproxy server.
stop(Reason) ->
    error_logger:info_msg(io_lib:format("~p~n",[Reason])),
    Res = application:stop(couchdbproxy),
    application:stop(couchbeam),
    application:stop(lhttpc),
    application:stop(ssl),
    application:stop(crypto),
    Res.
    
%% @spec get_app_env(Opt :: atom()) -> term()
%% @doc The official way to get the values set in couchdbproxy's configuration file.
%%      Will return the undefined atom if that option is unset.
get_app_env(Opt) -> get_app_env(Opt, undefined).

%% @spec get_app_env(Opt :: atom(), Default :: term()) -> term()
%% @doc The official way to get the values set in couchdbproxy's configuration file.
%%      Will return Default if that option is unset.
get_app_env(Opt, Default) ->
    case application:get_env(couchdbproxy, Opt) of
	{ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
	    {ok, [[Val | _]]} -> Val;
	    error       -> Default
        end
    end.
    
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.