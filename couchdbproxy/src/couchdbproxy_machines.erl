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

-module(couchdbproxy_machines).
-author('Benoît Chesneau <benoitc@e-engura.org').

-behaviour(gen_server).

-include("couchdbproxy.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([get_ip/1]).
         
start_link() ->
    gen_server:start_link({local, couchdbproxy_machines}, couchdbproxy_machines, [], []).
    
init([]) ->
    Machines = load_machines(),
    {ok, Machines}.


handle_cast(_Msg, State) ->
    {noreply, State}.    
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.

get_ip(NodeName) ->
    gen_server:call(couchdbproxy_machines, {ip, NodeName}).

handle_call({ip, NodeName}, _From, Machines) ->
    Ip = proplists:get_value(NodeName, Machines),
    {reply, Ip, Machines}.


%% spec load_machines() -> list
%% @doc load machines list
load_machines() ->
    ViewPid = couchbeam_db:query_view(couchdbproxy, {"couchdbproxy", "machines"}, []),
    {_, _, _, Rows} = couchbeam_view:parse_view(ViewPid),
    couchbeam_view:close_view(ViewPid),
    AllMachines = lists:foldl(fun(Row, MachinesAcc) ->
       {_Id, NodeName, Ip} = Row,
      [{NodeName, ?b2l(Ip)}|MachinesAcc]
    end, [], Rows),
    lists:reverse(AllMachines).