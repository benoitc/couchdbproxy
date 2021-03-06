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

-module(couchdbproxy_web).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-export([start/1, stop/0, loop/3]).
-export([host/1, absolute_uri/2]).

-include("couchdbproxy.hrl").

%% External API
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {BaseHostname, Options2} = get_option(hostname, Options1),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot, BaseHostname)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options2]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot, BaseHostname) ->
    HostName = host(Req),
    State = #proxy{mochi_req = Req,
                   socket    = Req:get(socket),
                   headers   = Req:get(headers),
                   method    = Req:get(method),
                   host      = HostName,
                   basename  = BaseHostname},
                   
    {HostName1, _, _} = mochiweb_util:partition(HostName, ":"),
	HostNameParts = lists:reverse([mochiweb_util:unquote(Part)
	                    || Part <- string:tokens(HostName1, ".")]),
	
	BaseHostNameParts = lists:reverse([mochiweb_util:unquote(Part1)
                    	|| Part1 <- string:tokens(BaseHostname, ".")]),
                    	
	case get_node(BaseHostNameParts, HostNameParts, State) of
	    not_found ->
		    Req:not_found();
	    {ui, State1} ->
	        couchdbproxy_ui:http_handler(State1);
	    {ok, State1} ->
	        couchdbproxy_revproxy:request(State1)
	end.

%% Internal API  
get_node([], [], State) ->
    {ui, State};
get_node([], HostRest, State) ->
    try_find_cname(HostRest, State);
get_node([Token|Rest], [Token|HostRest], State) ->
    get_node(Rest, HostRest, State);
get_node(_, _, State) ->
    try_find_alias(State).
    
try_find_cname(CNameParts, #proxy{mochi_req=Req}=State) ->
    RawPath = Req:get(raw_path),
    case find_cname(CNameParts, RawPath) of
	    ui -> {ui, State};
	    {NodeName, Path} ->
	        case couchdbproxy_routes:get_node(NodeName) of
                [MachineName, Port] ->
                    case build_proxy_url(MachineName, Port) of
                        {ok, ProxyUrl} ->
                            {Path0, _, _} = mochiweb_util:urlsplit_path(Path),
    	                    State1 = State#proxy{url        = ProxyUrl,
    	                                         path       = Path,
    	                                         path_parts = [list_to_binary(mochiweb_util:unquote(Part))
                                                                             || Part <- string:tokens(Path0, "/")],
    	                                         route      = {node, NodeName}},
    	                    {ok, State1};
    	                O -> O
    	            end;
                O -> O
            end
    end.
	
find_cname(["www"|[]], _RawPath) ->
    ui;
find_cname([NodeName|[]], RawPath) ->
    {NodeName, RawPath};
find_cname([NodeName, DbName|[]], RawPath) ->
    Path = lists:append(["/", DbName, RawPath]),
    {NodeName, Path};
find_cname([NodeName, DbName, DName|_], RawPath) ->
    Path = lists:append(["/", DbName, "/", "_design", "/", DName, RawPath]),
    {NodeName, Path}.
    	
try_find_alias(#proxy{mochi_req=Req, host=HostName}=State) ->
    RawPath = Req:get(raw_path),
    {HostName1, _, _} = mochiweb_util:partition(HostName, ":"),
    case couchdbproxy_routes:get_alias(HostName1) of
        [NodeName, Path] ->
            case couchdbproxy_routes:get_node(NodeName) of
                [MachineName, Port] ->
                    case build_proxy_url(MachineName, Port) of
                        {ok, ProxyUrl} ->
                            Path1 = case ?b2l(Path) of
                                "/" -> RawPath;
                                P -> lists:append([P, RawPath])
                            end,
                            {Path2, _, _} = mochiweb_util:urlsplit_path(Path1),
                            State1 = State#proxy{url          = ProxyUrl, 
                    	                         path         = Path2,
                    	                         path_parts   = [list_to_binary(mochiweb_util:unquote(Part))
                                                                        || Part <- string:tokens(Path2, "/")],
                    	                         route        = {cname, HostName}},
                            {ok, State1};
                        Other -> Other
                    end;
                Other -> Other
            end;
        O -> O
    end.
    
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

host(Req) ->
	case Req:get_header_value("Host") of
        undefined ->
            {ok, {Address, Port}} = inet:sockname(Req:get(socket)),
            inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port);
        Value -> Value
    end.

absolute_uri(Req, Path) ->
	Host = host(Req),
	"http://" ++ Host ++ Path.

build_proxy_url(MachineName, Port) ->
    case couchdbproxy_machines:get_ip(MachineName) of
        {ok, Ip} ->
            ProxyUrl  = "http://" ++ Ip ++ ":" ++ integer_to_list(Port),
            {ok, ProxyUrl};
        O -> O
    end.
    
