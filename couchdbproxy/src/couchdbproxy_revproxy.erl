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

-module(couchdbproxy_revproxy).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchdbproxy.hrl").

-export([request/1]).
    
-define(IDLE_TIMEOUT, infinity).
-define(STREAM_CHUNK_SIZE, 16384). %% 16384

request(State) ->
    try
        do_request(State)
    catch
        exit:{econnrefused, _} ->
            gateway_error(State, <<"couchdb node is down.">>)
    end.

do_request(#proxy{method=Method,url=Url,path=Path}=State) ->
    #url{host=Host, port=Port} = couchdbproxy_util:parse_url(Url),
    {ok, Headers} = proxy_headers(State),
    
    PartialDownload = options_partial_download(State),
    {Options, Body, Length} = case has_body(Method) of
        false -> {[{partial_download, PartialDownload}], <<>>, 0};
        true ->
            first_chunk(State)
    end,
    case lhttpc:request(Host, Port, false, Path, Method, Headers, Body, ?IDLE_TIMEOUT, Options) of
        {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}} ->
            State1 = State#proxy{
                status_code = StatusCode,
                reason      = ReasonPhrase,
                response_headers = Hdrs,
                response_body = ResponseBody
            },
            send_response(State1);
        {ok, UploadState} -> % we stream body
            case stream_body(State, UploadState, Length) of 
                {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}} ->
                    State1 = State#proxy{
                        status_code = StatusCode,
                        reason      = ReasonPhrase,
                        response_headers = Hdrs,
                        response_body = ResponseBody
                    },
                    send_response(State1);
                {error, Reason} ->
                    gateway_error(State, Reason)
            end;
        
        {error, Reason} ->
            gateway_error(State, Reason)
    end.

send_response(#proxy{mochi_req=MochiReq,url=Url,host=Host,status_code=Status,
            response_headers=RespHeaders,response_body=ResponseBody} = State) ->
    if
        (Status == 301) orelse (Status == 302) ->
            RedirectUrl = mochiweb_headers:get_value("Location",
                                    mochiweb_headers:make(RespHeaders)),
            RedirectUrl1 =  case mochiweb_util:partition(RedirectUrl, Url) of
                {"", _, RelPath} ->
                    couchdbproxy_web:absolute_uri(MochiReq, RelPath);
                {_, "", ""} -> RedirectUrl
            end,
            MochiReq:respond({Status, [{"Location", RedirectUrl1}], <<>>});
        true ->
            RespHeaders1 = fix_location(rewrite_headers(RespHeaders, Host), Host),
            {Resp, RespType} = case body_length(State) of
                chunked ->
                    {MochiReq:respond({Status, RespHeaders1 ++ couchdbproxy_http:server_headers(), chunked}), chunked};
                _ ->
                    {couchdbproxy_http:start_response(MochiReq, {Status, RespHeaders1}), normal}
            end,
            
            case ResponseBody of
                undefined ->                   
                    ok;
                Body when is_pid(Body) ->
                    InitialState = lhttpc:get_body_part(Body),
                    case RespType of
                        chunked ->
                            send_chunked_response(InitialState, Body, Resp);
                        _ ->
                            send_unchunked_response(InitialState, Body, Resp)
                    end;
                Body ->
                    case RespType of
                        chunked ->
                            Resp:write_chunk(Body),
                            Resp:write_chunk("");
                        _ ->
                            Resp:send(Body)
                    end
            end
        end.
        
send_chunked_response({ok, {http_eob, _Trailers}}, _Pid, Resp) ->
    Resp:write_chunk("");
send_chunked_response({ok, Bin}, Pid, Resp) ->
    Resp:write_chunk(Bin),
    NextState = lhttpc:get_body_part(Pid),
    send_chunked_response(NextState, Pid, Resp).
       
send_unchunked_response({ok, {http_eob, _Trailers}}, _Pid, _Resp) ->
    ok;
send_unchunked_response({ok, Bin}, Pid, Resp) ->
    Resp:send(Bin),
    NextState = lhttpc:get_body_part(Pid),
    send_unchunked_response(NextState, Pid, Resp).           
            
body_length(#proxy{response_headers=Hdrs}) ->
    MHdrs = mochiweb_headers:make(Hdrs),
	case mochiweb_headers:get_value("transfer-encoding", MHdrs) of
	undefined ->
		case mochiweb_headers:get_value("content-length", MHdrs) of
		undefined -> undefined;
		Length -> list_to_integer(Length)
		end;
	"chunked" -> chunked;
    Unknown -> {unknown_transfer_encoding, Unknown}
    end.

stream_body(#proxy{mochi_req=MochiReq}=State, UploadState, Length) ->
    NextState = case MochiReq:body_length() of
        chunked ->
            send_chunked_body(State, UploadState);
        _ ->
            send_unchunked_body(State, UploadState, Length)
    end,
    lhttpc:send_body_part(NextState, http_eob).

send_chunked_body(#proxy{mochi_req=MochiReq}=State, CurrentState) ->
    case MochiReq:read_chunk_length() of
    0 ->
        Chunk = MochiReq:read_chunk(0),
        {ok, NextState} = lhttpc:send_body_part(CurrentState, Chunk),
        NextState;
    Length when Length > ?STREAM_CHUNK_SIZE ->
        Data = read_sub_chunks(Length, MochiReq, []),
        Data1 = iolist_to_binary(lists:reverse(Data)),
        {ok, NextState1} = lhttpc:send_body_part(CurrentState, Data1),
        send_chunked_body(State, NextState1);
    Length ->
        Data2 = MochiReq:read_chunk(Length),
        {ok, NextState2} = lhttpc:send_body_part(CurrentState, Data2),
        send_chunked_body(State, NextState2)
    end.

    
read_sub_chunks(Length, Req, Acc) when Length > ?STREAM_CHUNK_SIZE ->
    Bin = Req:recv(?STREAM_CHUNK_SIZE),
    read_sub_chunks(Length - ?STREAM_CHUNK_SIZE, Req, [Bin|Acc]);

read_sub_chunks(Length, Req, Acc) ->
    Bin = Req:read_chunk(Length),
    [Bin|Acc].

send_unchunked_body(#proxy{socket=Socket}=State, CurrentState, DataLeft) ->
    lhttpc_sock:setopts(Socket, [{packet, raw}], false),
    case ?STREAM_CHUNK_SIZE >= DataLeft of
        true ->
            {ok, Data1} = lhttpc_sock:recv(Socket, DataLeft, false),
            {ok, NextState} = lhttpc:send_body_part(CurrentState, Data1),
            NextState;
        false ->
            {ok, Data2} = lhttpc_sock:recv(Socket, ?STREAM_CHUNK_SIZE, false),
            {ok, NextState2} = lhttpc:send_body_part(CurrentState, Data2),
            send_unchunked_body(State, NextState2, DataLeft-?STREAM_CHUNK_SIZE)
    end.
    
first_chunk(#proxy{mochi_req=MochiReq}=State) ->
    PartialDownload = options_partial_download(State),
    Options = [{partial_upload, infinity}, {partial_download, PartialDownload}],
    case MochiReq:body_length() of
        {unknown_transfer_encoding, _Unknown} ->
            {[], <<>>, 0};
        undefined ->
            {[], <<>>, 0};
        0 -> 
            {[], <<>>, 0};
        chunked ->
            case MochiReq:read_chunk_length() of
                undefined ->
                    {[], <<>>, 0};
                0 ->
                    Chunk = MochiReq:read_chunk(0),
                    {[], Chunk, 0};
                Length ->
                    Chunk = MochiReq:read_chunk(Length),
                    {Options, Chunk, undefined}
            end;
        Length when Length == ?STREAM_CHUNK_SIZE orelse Length < ?STREAM_CHUNK_SIZE ->
            Body = MochiReq:recv(Length),
            {[{partial_download, PartialDownload}], Body, 0};
        Length ->
            Body = MochiReq:recv(?STREAM_CHUNK_SIZE),
            Length1 = Length - ?STREAM_CHUNK_SIZE,
            {Options, Body, Length1}
    end.
            
rewrite_headers(Hdrs, Host) ->
    MHdrs = mochiweb_headers:make(Hdrs),
    mochiweb_headers:to_list(mochiweb_headers:enter("Host", Host, MHdrs)).
    
fix_location([],_) -> [];
fix_location([{"Location", CouchDataPath}|Rest],
             ExternalPath) ->
  	{Scheme, _Netloc, Path, Query, Fragment} = mochiweb_util:urlsplit(CouchDataPath),
	NewPath = mochiweb_util:urlunsplit({Scheme, ExternalPath, Path, Query, Fragment}),
    [{"Location", NewPath}|Rest];
fix_location([H|T], C) ->
    [H|fix_location(T, C)].
    
proxy_headers(#proxy{mochi_req=MochiReq,host=ProxyHost, headers=Hdrs, url=Url}) ->
    #url{host=Host, port=Port} = couchdbproxy_util:parse_url(Url),
    RemoteAddr = MochiReq:get(peer),
    % set default proxy headers, useful for oauth & co
	ProxyHeaders = [{"X-Forwarded-For", RemoteAddr}, {"X-Forwarded-Host", ProxyHost}],
	% rewrite headers
    Hdrs1 = mochiweb_headers:enter("Host", lists:append([Host, ":", integer_to_list(Port)]), Hdrs),
    Hdrs2 = mochiweb_headers:default_from_list(ProxyHeaders, Hdrs1),
    Hdrs3 = [{maybe_atom(Name),Value} || {Name,Value} <- mochiweb_headers:to_list(Hdrs2)],
    {ok, Hdrs3}.
    
gateway_error(#proxy{route=Route}=State, Reason) ->
    couchdbproxy_routes:clean_route(Route),
    couchdbproxy_http:send_error(State, {bad_gateway, Reason}).


options_partial_download(#proxy{path_parts=[_DbName, <<"_changes">>|_]}) ->
    [{window_size, infinity}];
options_partial_download(#proxy{path_parts=[_DbName, <<"_active_tasks">>|_]}) ->
    [{window_size, infinity}];
options_partial_download(_) ->
    [{window_size, infinity}, {part_size, ?STREAM_CHUNK_SIZE}].
    
maybe_atom(K) when is_atom(K) ->
    atom_to_list(K);
maybe_atom(K) ->
    K.
	
has_body('HEAD') ->
    false;
has_body('GET') ->
    false;
has_body('DELETE') ->
    false;
has_body(_) ->
    true.