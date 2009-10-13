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
-export([get_alias/1, get_user/1, clean_user/1, clean_route/1]).
         
-record(routes,{
    aliases,
    users}).         
         
start_link() ->
    gen_server:start_link({local, couchdbproxy_routes}, couchdbproxy_routes, [], []).
    
init([]) ->
    Aliases = ets:new(couchdbproxy_aliases, [set, protected]),
    Users = ets:new(couchdbproxy_users, [set, protected]),
    Routes = #routes{aliases=Aliases, users=Users},
    {ok, Routes}.

get_alias(HostName) ->
    gen_server:call(couchdbproxy_routes, {get_alias, HostName}).
    
get_user(UserName) ->
    gen_server:call(couchdbproxy_routes, {get_user, UserName}).
    
clean_user(UserName) ->
    gen_server:cast(couchdbproxy_routes, {clean_user, UserName}).
    
clean_cname(HostName) ->
    gen_server:cast(couchdbproxy_routes, {clean_cname, HostName}).
    
clean_route({user, UserName}) ->
    clean_user(UserName);
clean_route({cname, HostName}) ->
    clean_cname(HostName).
    
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
    
handle_call({get_user, UserName}, _From, #routes{users=Users}=Routes) ->
    R = case ets:lookup(Users, UserName) of
    [] ->
        ViewPid = couchbeam_db:query_view(couchdbproxy, {"couchdbproxy", "usernode"},
                        [{"key", ?l2b(UserName)}, {"limit", 1}]),
        case couchbeam_view:parse_view(ViewPid) of
        {_, _, _, []} -> 
            couchbeam_view:close_view(ViewPid),
            not_found;
        {_, _, _, [{_, _, V}]} ->
            couchbeam_view:close_view(ViewPid),
            ets:insert(Users, {UserName, V}),
            V
        end;
    [{UserName, V1}] -> V1
    end,
    {reply, R, Routes}.
    
handle_cast({clean_cname, HostName}, #routes{aliases=Aliases}=Routes) ->
    Key = [list_to_binary(mochiweb_util:unquote(Part))
	 		|| Part <- string:tokens(HostName, ".")],
    case ets:lookup(Aliases, Key) of
        [] -> ok;
        [{Key, _V1}] ->
            ets:delete(Aliases, Key)
    end,
    {noreply, Routes};

handle_cast({clean_user, UserName}, #routes{users=Users}=Routes) ->
    case ets:lookup(Users, UserName) of
        [] -> ok;
        [{UserName, _V1}] ->
        ets:delete(Users, UserName)
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