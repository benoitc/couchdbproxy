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

-module(couchdbproxy_listener).
-author('Benoît Chesneau <benoitc@e-engura.org').

-behaviour(gen_server).

-include("couchdbproxy.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([changes_loop/1]).


-record(listener, {
    changes_pid=undefined,
    loop_pid=undefined,
    since=nil,
    fd,
    retry=0
}).

              
start_link(Args) ->
    gen_server:start_link({local, couchdbproxy_listener}, couchdbproxy_listener, Args, []).
    
init(FileName) ->
    Since = case file:read_file(FileName) of
        {ok, <<>>} -> 0;
        {ok, Bin} -> list_to_integer(binary_to_list(Bin));
        _ -> 0
    end,
    {ok, Fd} = file:open(FileName, [write]),
    
    start_loop(Since),
    {ok, #listener{fd=Fd, since=Since}}.
    
    
start_loop(Since) ->
    Server = self(),
    LoopPid = proc_lib:spawn_link(?MODULE, changes_loop, [Server]),
    Pid = couchbeam_db:suscribe(couchdbproxy, LoopPid, [{heartbeat, "true"}, {since, Since}]),
    Server ! {listen, LoopPid, Pid},
    ok.
    
changes_loop(Server) ->
    receive
        {body_pid, Pid} ->
            erlang:monitor(process, Pid),
            Server ! start_listen,
            changes_loop(Server);
        {change, Change} ->
            io:format("got change ~p ~n", [Change]),
            spawn_link(fun() -> parse_change(Change, Server) end),
            changes_loop(Server);
        {'DOWN', _, _, _, _} ->
            gen_server:cast(Server, restart_loop);
        {'EXIT', {econnrefused, _}} ->
            gen_server:cast(Server, restart_loop);
        Else ->
            io:format("ici ~p ~n", [Else])
    end.

    
handle_call(_Msg, _, State) ->
    {reply, ok, State}.

handle_cast(restart_loop, #listener{since=Since,retry=Wait}=State) ->
    timer:sleep(Wait),
    start_loop(Since),
    {noreply, State#listener{retry=Wait+5000}};
    
handle_cast(_Msg, State) ->
    {noreply, State}.    
    
handle_info({listen, LoopPid, ChangePid}, State) ->
    {noreply, State#listener{changes_pid=ChangePid, loop_pid=LoopPid}};
handle_info({new_change, Seq}, #listener{fd=Fd}=State) ->
    file:pwrite(Fd, 0, list_to_binary(integer_to_list(Seq))),
    file:sync(Fd),
    {noreply, State#listener{since=Seq}};
handle_info(start_listen, State) ->
    {noreply, State#listener{retry=0}};
handle_info(_Info, State) ->
    io:format("got ~p ~n", [_Info]),
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, #listener{fd=Fd}) ->
    file:close(Fd),
    ok.
       
parse_change([{Props}], Server) ->
    Seq = proplists:get_value(<<"seq">>, Props),
    Server ! {new_change, Seq},
    Id = proplists:get_value(<<"id">>, Props),
    Deleted = proplists:get_value(<<"deleted">>, Props, false),
    [{Changes}] = proplists:get_value(<<"changes">>, Props),
    {DocProps} = case proplists:get_value(<<"rev">>, Changes) of
        undefined ->
            couchbeam_db:open_doc(couchdbproxy, binary_to_list(Id));
        Rev ->
            couchbeam_db:open_doc(couchdbproxy, binary_to_list(Id), [{rev, Rev}])
    end,
    
    case proplists:get_value(<<"type">>, DocProps) of
        undefined -> ok;
        <<"machine">> ->
            Ip = proplists:get_value(<<"ips">>, DocProps, <<"127.0.0.1">>),
            Name = proplists:get_value(<<"name">>, DocProps),
            case Deleted of
                true ->
                    couchdbproxy_machines:delete_machine(Name);
                false ->
                    couchdbproxy_machines:add_machine(Name, Ip)
            end;
        <<"node">> ->
            NodeName = proplists:get_value(<<"nodename">>, DocProps),
            MachineName = proplists:get_value(<<"machine">>, DocProps),
            Port = proplists:get_value(<<"port">>, DocProps),
            Active = proplists:get_value(<<"active">>, DocProps),
            case Deleted of
                true ->
                    couchdbproxy_routes:clean_node(NodeName);
                false ->
                    case Active of
                        true ->
                            couchdbproxy_routes:update_node(NodeName, MachineName, Port);
                        false ->
                            couchdbproxy_routes:clean_node(NodeName)
                    end
            end;
        <<"alias">> ->
            NodeName = proplists:get_value(<<"nodename">>, DocProps),
            HostName = proplists:get_value(<<"hostname">>, DocProps),
            Path = proplists:get_value(<<"path">>, DocProps),
            case Deleted of
                true ->
                    couchdbproxy_routes:clean_cname(HostName);
                false ->
                    couchdbproxy_routes:update_alias(HostName, NodeName, Path)
            end;
        _ -> ok
    end.