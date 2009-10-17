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
-export([get_ip/1, add_machine/2, delete_machine/1]).


-record(machines_state, {
    machines=dict:new()
}).

get_ip(Name) ->
    gen_server:call(couchdbproxy_machines, {ip, Name}).
    
add_machine(Name, Ip) ->
    gen_server:cast(couchdbproxy_machines, {add_machine, Name, Ip}).
    
delete_machine(Name) ->
    gen_server:cast(couchdbproxy_machines, {delete_machine, Name}).
    
              
start_link() ->
    gen_server:start_link({local, couchdbproxy_machines}, couchdbproxy_machines, [], []).
    
init([]) ->
    InitialState = load_machines(),
    {ok, InitialState}.

handle_call({ip, Name}, _From, #machines_state{machines=Machines}=State) ->
    Ip = case dict:find(Name, Machines) of
        {ok, Ip1} -> {ok, Ip1};
        error -> not_found
    end,
    {reply, Ip, State}.

handle_cast({add_machine, Name, Ip}, #machines_state{machines=Machines}=State) ->
    Machines1 = dict:store(Name, binary_to_list(Ip), Machines),
    {noreply, State#machines_state{machines=Machines1}};
    
handle_cast({delete_machine, Name}, #machines_state{machines=Machines}=State) ->
    Machines1 = case dict:find(Name, Machines) of
        {ok, _} ->
            dict:erase(Name, Machines);
        error ->
            Machines
    end,
    {noreply, State#machines_state{machines=Machines1}};
    
handle_cast(_Msg, State) ->
    {noreply, State}.    
    
handle_info(_Info, State) ->
    io:format("got ~p ~n", [_Info]),
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.

%% spec load_machines() -> list
%% @doc load machines list
load_machines() ->
    ViewPid = couchbeam_db:query_view(couchdbproxy, {"couchdbproxy", "machines"}, []),
    {_, _, _, Rows} = couchbeam_view:parse_view(ViewPid),
    couchbeam_view:close_view(ViewPid),
    AllMachines = lists:foldl(fun(Row, MachinesAcc) ->
       {_Id, Name, Ip} = Row,
      [{Name, binary_to_list(Ip)}|MachinesAcc]
    end, [], Rows),
    Machines = dict:from_list(lists:reverse(AllMachines)),
    State = #machines_state{machines=Machines},
    State.