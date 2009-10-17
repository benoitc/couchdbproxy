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

-module(couchdbproxy_routes).
-author('Benoît Chesneau <benoitc@e-engura.org').

-behaviour(gen_server).

-include("couchdbproxy.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([get_alias/1, get_node/1, clean_node/1, clean_route/1,
         update_alias/3, update_node/3]).
         
-record(routes,{
    aliases,
    nodes}).         
         
start_link() ->
    gen_server:start_link({local, couchdbproxy_routes}, couchdbproxy_routes, [], []).
    
init([]) ->
    Aliases = ets:new(couchdbproxy_aliases, [set, protected]),
    Nodes = ets:new(couchdbproxy_nodes, [set, protected]),
    Routes = #routes{aliases=Aliases, nodes=Nodes},
    {ok, Routes}.

get_alias(HostName) ->
    gen_server:call(couchdbproxy_routes, {get_alias, HostName}).
    
get_node(NodeName) when is_list(NodeName) ->
    get_node(?l2b(NodeName));    
get_node(NodeName) ->
    gen_server:call(couchdbproxy_routes, {get_node, NodeName}).
    
clean_node(NodeName) ->
    gen_server:cast(couchdbproxy_routes, {clean_ndoe, NodeName}).
    
clean_cname(HostName) ->
    gen_server:cast(couchdbproxy_routes, {clean_cname, HostName}).
    
clean_route({node, NodeName}) ->
    clean_node(NodeName);
clean_route({cname, HostName}) ->
    clean_cname(HostName).
    
update_alias(HostName, NodeName, Path) ->
    gen_server:cast(couchdbproxy_routes, {set_alias, HostName, NodeName, Path}).
    
update_node(NodeName, MachineName, Port) ->
    gen_server:cast(couchdbproxy_routes, {set_node, NodeName, MachineName, Port}).
    
handle_call({get_alias, HostName}, _From, #routes{aliases=Aliases}=Routes) ->
    Key = [list_to_binary(mochiweb_util:unquote(Part))
	 		|| Part <- string:tokens(HostName, ".")],
	R = case ets:lookup(Aliases, Key) of
	[] ->
	    ViewPid = couchbeam_db:query_view(couchdbproxy, {"couchdbproxy", "alias"}, 
	                    [{"key", Key}, {"limit", 1}]),
        case couchbeam_view:parse_view(ViewPid) of
        {_, _, _, []} -> 
            couchbeam_view:close_view(ViewPid),
            not_found;
        {_, _, _, [{_, _, V}]} ->
            ets:insert(Aliases, {Key, V}),
            couchbeam_view:close_view(ViewPid),
            V
        end;
    [{Key, V1}] -> V1
    end,
    {reply, R, Routes};
    
handle_call({get_node, NodeName}, _From, #routes{nodes=Nodes}=Routes) ->
    R = case ets:lookup(Nodes, NodeName) of
    [] ->
        ViewPid = couchbeam_db:query_view(couchdbproxy, {"couchdbproxy", "nodes_byname"},
                        [{"key", NodeName}, {"limit", 1}]),
        case couchbeam_view:parse_view(ViewPid) of
        {_, _, _, []} -> 
            couchbeam_view:close_view(ViewPid),
            not_found;
        {_, _, _, [{_, _, V}]} ->
            couchbeam_view:close_view(ViewPid),
            ets:insert(Nodes, {NodeName, V}),
            V
        end;
    [{NodeName, V1}] -> V1
    end,
    {reply, R, Routes}.
    
handle_cast({set_node, NodeName, MachineName, Port}, #routes{nodes=Nodes}=State) ->
    ets:insert(Nodes, {NodeName, [MachineName, Port]}),
    {noreply, State};
    
handle_cast({set_alias, HostName, NodeName, Path}, #routes{aliases=Aliases}=State) ->
    ets:insert(Aliases, {HostName, [NodeName, Path]}),
    {noreply, State};    
    
handle_cast({clean_cname, HostName}, #routes{aliases=Aliases}=Routes) ->
    Key = [list_to_binary(mochiweb_util:unquote(Part))
	 		|| Part <- string:tokens(HostName, ".")],
    case ets:lookup(Aliases, Key) of
        [] -> ok;
        [{Key, _V1}] ->
            ets:delete(Aliases, Key)
    end,
    {noreply, Routes};

handle_cast({clean_node, NodeName}, #routes{nodes=Nodes}=Routes) ->
    case ets:lookup(Nodes, NodeName) of
        [] -> ok;
        [{NodeName, _V1}] ->
        ets:delete(Nodes, NodeName)
    end,
    {noreply, Routes};
        
handle_cast(_Msg, State) ->
    {noreply, State}.    

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% TODO : allow deletion of routes
terminate(_Reason, _State) ->
    ok.