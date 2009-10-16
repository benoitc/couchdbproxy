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

-module(couchdbproxy_sup).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("couchbeam.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    
    {ok, ProxyConf} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "proxy.conf"])),
                          
    Ip = case proplists:get_value(ip, ProxyConf) of
        undefined ->
            case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end;
        Ip1 -> Ip1
    end,
    Port = proplists:get_value(port, ProxyConf, 8000),
                       
    BaseHostname = proplists:get_value(hostname, ProxyConf, "couchdbproxy.dev"),
    WebConfig = [
         {ip, Ip},
                 {port, Port},
                 {docroot, couchdbproxy_deps:local_path(["priv", "www"])},
                 {hostname, BaseHostname}],

    Params = case proplists:get_value(proxy_hostconfig, ProxyConf) of
        undefined -> #couchdb_params{};
        HostConfig ->
            #couchdb_params{
                host=proplists:get_value(host, HostConfig, "127.0.0.1"),
                port=proplists:get_value(port, HostConfig, 5984),
                username=proplists:get_value(username, HostConfig, nil),
                password=proplists:get_value(password, HostConfig, nil),
                name=couchdbproxy
            }
    end,
    ProxyDb = proplists:get_value(proxy_db, ProxyConf, "couchdbproxy"),
    _ConnectionPid = couchbeam_server:start_connection(Params),
    couchbeam_server:open_db(couchdbproxy, {couchdbproxy, ProxyDb}),

    Processes = [{couchdbproxy_web,
                    {couchdbproxy_web, start, [WebConfig]},
                    permanent, 5000, worker, dynamic},
                {couchdbproxy_machines,
                    {couchdbproxy_machines, start_link, []},
                    permanent, 5000, worker, [couchdbproxy_machines]},
                {couchdbproxy_routes,
                    {couchdbproxy_routes, start_link, []},
                    permanent, 5000, worker, [couchdbproxy_routes]}],
    {ok, {{one_for_one, 10, 10}, Processes}}.
