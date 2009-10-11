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
           
    io:format("ici"),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options2]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot, BaseHostname) ->
	case get_cname(Req, BaseHostname) of
	    not_found ->
		    Req:not_found();
	    {raw, Body} ->
	        Headers = [{"Content-Type", "application/json"}],
	        Req:respond({200, couchdbproxy_http:server_headers(), Body});
		{user, UserName, ProxyUrl, Path} -> 
			%% revert cname parts so we could use it for proxy and rewrite
			ProxyReq = #proxy{
				mochi_req=Req
			},
			couchdbproxy_revproxy:request(ProxyReq, Path, ProxyUrl, self()),
			receive
			    error ->
			        couchdbproxy_routes:clean_user(UserName)
			after 0 -> ok
			end;
	    {cname,  HostName, ProxyUrl, Path} ->
	        %% revert cname parts so we could use it for proxy and rewrite
			ProxyReq = #proxy{
				mochi_req=Req
			},
			couchdbproxy_revproxy:request(ProxyReq, Path, ProxyUrl, self()),
		    receive
			    error ->
			        couchdbproxy_routes:clean_cname(HostName)
			after 0 -> ok
			end
	end.

%% Internal API
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

get_cname(Req, BaseName) ->
    HostName = host(Req),
    RawPath = Req:get(raw_path),
	%% in case there is a port
	{HostName1, _, _} = mochiweb_util:partition(HostName, ":"),
	CNameRe = lists:append(["^(.*).", BaseName, "$"]),
	{ok, R} = re:compile(CNameRe),
	case re:run(HostName1, R, [{capture,[1], list}]) of 
	{match, [CName]} -> 
	    CNameParts = lists:reverse([mochiweb_util:unquote(Part)
		 		|| Part <- string:tokens(CName, ".")]),
		case find_cname(CNameParts, RawPath) of
		    {raw, Body} -> {raw, Body};
		    {UserName, Path} ->
		        case couchdbproxy_routes:get_user(UserName) of
	                [NodeName, Port] ->
	                    ProxyUrl = build_proxy_url(NodeName, Port),
	                    {user, UserName, ProxyUrl, Path};
	                O -> O
                end
        end;
	_ ->
	    if 
	        HostName1 =:= BaseName ->
	            welcome_response();
	        true ->
	            find_alias(HostName1, RawPath)
	    end
	end.
	
find_cname(["www"|[]], _RawPath) ->
    welcome_response();
find_cname([UserName|[]], RawPath) ->
    {UserName, RawPath};
find_cname([UserName, DbName|[]], RawPath) ->
    Path = lists:append(["/", DbName, RawPath]),
    {UserName, Path};
find_cname([UserName, DbName, DName|_], RawPath) ->
    Path = lists:append(["/", DbName, "/", "_design", "/", DName, RawPath]),
    {UserName, Path}.
    	
find_alias(HostName, RawPath) ->
    case couchdbproxy_routes:get_alias(HostName) of
    [NodeName, Port, Path] ->
        ProxyUrl = build_proxy_url(NodeName, Port),
        Path1 = case ?b2l(Path) of
        "/" -> RawPath;
        P -> lists:append([P, RawPath])
        end,
        {cname, HostName, ProxyUrl, Path1};
    O -> O
    end.
    
build_proxy_url(NodeName, Port) ->
    Ip = couchdbproxy_nodes:get_ip(NodeName),
    ProxyUrl  = "http://" ++ Ip ++ ":" ++ integer_to_list(Port),
    ProxyUrl.
    
welcome_response() ->
    {raw, couchbeam:json_encode({[
        {couchdbproxy, <<"Welcome">>},
        {version, list_to_binary(io_lib:format("~s/~s", [?QUIP, ?CPVSN]))}
    ]})}.