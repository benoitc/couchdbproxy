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

-export([request/4]).

-record(psock, {
    socket,
    headers,
    attempts}).
    
-define(IDLE_TIMEOUT, infinity).
-define(STREAM_CHUNK_SIZE, 16384). %% 16384

request(From, Req, Path, Url) ->
    R = try
        execute(Req, Path, Url)
    catch
        
        error: Error ->
            From ! error,
            couchdbproxy_http:send_error(Req, {bad_gateway, Error})
    end,
    unlink(From),
    ok.
        
        
execute(#proxy{mochi_req=MochiReq}= Req, Path, Url) ->
    #url{host=Host, port=Port} = couchdbproxy_util:parse_url(Url),
    
    SocketRequest = {socket, self(), Host, Port, false},
    DSock = case gen_server:call(lhttpc_manager, SocketRequest, infinity) of
        {ok, S}   -> S; % Re-using HTTP/1.1 connections
        no_socket -> undefined % Opening a new HTTP/1.1 connection
    end,
    From = #psock{socket=MochiReq:get(socket), headers=MochiReq:get(headers)},
    To = #psock{socket=DSock, attempts=2},
    case do_proxy_request(Req, From, To, Path, Url) of
        {ok, undefined} -> ok;
        {ok, NewSocket} ->
            ManagerPid = whereis(lhttpc_manager),
            case lhttpc_sock:controlling_process(NewSocket, ManagerPid, false) of
                ok ->
                    gen_server:cast(lhttpc_manager,
                        {done, Host, Port, false, NewSocket});
                _ ->
                    ok
            end
    end,
    ok.
    
    

do_proxy_request(_Req, _From, #psock{attempts=0}, _Path, _Url) ->
    throw(connection_closed);
do_proxy_request(Req, From, #psock{socket=undefined}=To, Path, Url) ->
    #url{host=Host, port=Port} = couchdbproxy_util:parse_url(Url),
    SocketOptions = [binary, {packet, http}, {active, false}, {nodelay, true}],
    case lhttpc_sock:connect(Host, Port, SocketOptions, 300000, false) of
        {ok, Socket} ->
            do_proxy_request(Req, From, To#psock{socket=Socket}, Path, Url);
        {error, etimedout} ->
            % TCP stack decided to give up
            throw(connect_timeout);
        {error, timeout} ->
            throw(connect_timeout);
        {error, Reason} ->
            erlang:error(Reason)
    end;
 
do_proxy_request(#proxy{mochi_req=MochiReq}=Req, From, To, Path, Url) ->
    Headers = mochiweb_headers:to_list(MochiReq:get(headers)),
    Method = convert_method(MochiReq:get(method)),
    case start_client_request(Req, To#psock.socket, Method, Path, Headers, Url) of
    ok -> 
        case MochiReq:get_header_value("expect") of
    	{"100-continue", _} ->
    	    MochiReq:start_raw_response({100, gb_trees:empty()});
    	_Else ->
    	    ok
        end,
        case recv_stream_body(From, To, ?STREAM_CHUNK_SIZE) of
        {error, Reason1} ->
            couchdbproxy_http:send_error(Req, {bad_request, Reason1}),
            exit(normal);
        _ -> ok
        end,
        {R, RespHeaders} = get_proxy_headers(To#psock.socket),
       
        #http_response{status=Status} = R,
        if
        (Status == 301) orelse (Status == 302) ->
            RedirectUrl = mochiweb_headers:get_value("Location",
                 mochiweb_headers:make(RespHeaders)),
            RedirectUrl1 =  case mochiweb_util:partition(RedirectUrl, Url) of
                {"", _, RelPath} ->
         		    couchdbproxy_web:absolute_uri(MochiReq, RelPath);
                {_, "", ""} -> RedirectUrl
                end,
            couchdbproxy_http:send_redirect(Req, RedirectUrl1);
        true ->
            Host = couchdbproxy_web:host(MochiReq),
            RespHeaders1 = fix_location(rewrite_headers(RespHeaders, Host), Host),
            To1 = #psock{
                    socket=To#psock.socket,
                    headers=mochiweb_headers:make(RespHeaders1)},
            case body_length(To1) of
                chunked -> couchdbproxy_http:start_response_chunked(MochiReq, {R, RespHeaders1});
                _ -> couchdbproxy_http:start_response(MochiReq, {R, RespHeaders1})
            end,
            
            case get_body(Method, Status) of
            true -> 
                case recv_stream_body(To1, From, ?STREAM_CHUNK_SIZE) of
                {error, _Reason2} -> 
                    write_chunk(To1#psock.socket, "<< an unexpected error happened >>");
                ok -> 
                    case body_length(To1) of
                    chunked ->  write_chunk(To1#psock.socket, "");
                    _ -> ok
                    end
                end;      
            false -> 
                ok 
            end
            
        end,
        {ok, To#psock.socket};
    {error, Reason} ->
        throw(Reason),
        couchdbproxy_http:send_error(Req, {bad_gateway, Reason})
    end.

get_body(Method, Status) ->
    if 
    Method == "HEAD" -> false;
    true ->
        case lists:member(Status,  [100,204,205,304]) of
        true -> false;
        false -> true
        end
    end.
    
collect_headers (Socket, Resp, Headers, Count) when Count < 1000 ->
    case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, {http_header, _Num, Name, _, Value}} ->
            collect_headers(Socket, Resp, [{Name, Value} | Headers], Count+1);
        {ok, http_eoh} ->
            Headers;
        {error, {http_error, "\r\n"}} ->
            collect_headers(Socket, Resp, Headers, Count+1);
        {error, {http_error, "\n"}} ->
            collect_headers(Socket, Resp, Headers, Count+1);
        _Err ->
            exit(normal) 
    end.

get_proxy_headers(Socket) ->
    lhttpc_sock:setopts(Socket, [{packet, http}], false),
    case http_recv_request(Socket) of
        bad_request ->
            {error, {bad_gateway, <<"Bad requesr">>}};
        closed ->
            {error, {bad_gateway, <<"Remote connection closed">>}};
        R -> 
            H = collect_headers(Socket, R, [], 0),
            {R, H}
    end.
    
http_recv_request(Socket) ->
    case lhttpc_sock:recv(Socket, 0, ?IDLE_TIMEOUT, false) of
        {ok, R} when is_record(R, http_request) ->
            
            R;
        {ok, R} when is_record(R, http_response) ->
            R;
        {error, {http_error, "\r\n"}} ->
            http_recv_request(Socket);
        {error, {http_error, "\n"}} ->
            http_recv_request(Socket);
        {error, {http_error, _}} ->
            bad_request;
        {error, closed} -> 
            closed;
        {error, timeout} -> closed;
        _Other ->
            io:format("Got ~p~n", [_Other]),
            exit(normal)
    end.

body_length(#psock{headers=H}) ->
	case mochiweb_headers:get_value("transfer-encoding", H) of
	undefined ->
		case mochiweb_headers:get_value("content-length", H) of
		undefined -> undefined;
		Length -> list_to_integer(Length)
		end;
	"chunked" -> chunked;
    Unknown -> {unknown_transfer_encoding, Unknown}
    end.

recv_stream_body(From, To, MaxHunkSize) ->
    case body_length(From) of
    {unknown_transfer_encoding, _} -> 
        lhttpc_sock:send(To#psock.socket, <<>>, false),
        ok;
    undefined ->
        lhttpc_sock:send(To#psock.socket, <<>>, false),
        ok;
    0 ->
        lhttpc_sock:send(To#psock.socket, <<>>, false),
        ok;
    chunked ->
        recv_chunked_body(From#psock.socket, To#psock.socket, MaxHunkSize);
    Length ->
        recv_unchunked_body(From#psock.socket, To#psock.socket, MaxHunkSize, Length)
    end.

recv_unchunked_body(From, To, MaxHunk, DataLeft) ->
    lhttpc_sock:setopts(From, [{packet, raw}], false),
    case MaxHunk >= DataLeft of
        true ->
            {ok,Data1} = lhttpc_sock:recv(From, DataLeft, ?IDLE_TIMEOUT, false),
            lhttpc_sock:send(To, Data1, false),
            ok;
        false ->
            {ok,Data2} = lhttpc_sock:recv(From, MaxHunk, ?IDLE_TIMEOUT, false),
            lhttpc_sock:send(To, Data2, false),
            recv_unchunked_body(From, To, MaxHunk, DataLeft-MaxHunk)
    end.
    
recv_chunked_body(From, To, MaxChunkSize) ->
    case read_chunk_length(From) of
    error -> 
        {error, "Bad chunked transfer-encoding header"};
    0 ->  
        write_chunk(To, read_chunk(From, 0)),
        ok;
    Length ->
        recv_chunked_body(From, To, MaxChunkSize, Length)
    end.

recv_chunked_body(From, To, MaxChunkSize, LeftInChunk) ->
    case MaxChunkSize >= LeftInChunk of
    true ->
        Data1 = read_chunk(From, LeftInChunk),
        write_chunk(To, Data1),
        recv_chunked_body(From, To, MaxChunkSize);
    false ->
        {ok, Data2} = lhttpc_sock:recv(From, MaxChunkSize,
            ?IDLE_TIMEOUT, false),
         write_chunk(To, Data2),
         recv_chunked_body(From, To, MaxChunkSize, LeftInChunk-MaxChunkSize)
    end.
    
read_chunk_length(Socket) ->
    lhttpc_sock:setopts(Socket, [{packet, line}], false),
    case lhttpc_sock:recv(Socket, 0, ?IDLE_TIMEOUT, false) of
        {ok, Header} ->
            lhttpc_sock:setopts(Socket, [{packet, raw}], false),
            Splitter = fun (C) ->
                               C =/= $\r andalso C =/= $\n andalso C =/= $
                       end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            
            %% catch badly encoded requests
            try mochihex:to_int(Hex)
            catch 
                _:_ -> error
            end;

        _ ->
            exit(normal)
    end.

read_chunk(Socket,  0) ->
    lhttpc_sock:setopts(Socket, [{packet, line}], false),
    F = fun (F1, Acc) ->
                case lhttpc_sock:recv(Socket, 0, ?IDLE_TIMEOUT, false) of
                    {ok, <<"\r\n">>} ->
                        Acc;
                    {ok, Footer} ->
                        F1(F1, [Footer | Acc]);
                    _ ->
                        exit(normal)
                end
        end,
    Footers = F(F, []),
    lhttpc_sock:setopts(Socket, [{packet, raw}], false),
    Footers;
read_chunk(Socket, Length) ->
    case lhttpc_sock:recv(Socket, 2 + Length, ?IDLE_TIMEOUT, false) of
        {ok, <<Chunk:Length/binary, "\r\n">>} ->
            Chunk;
        _ ->
            exit(normal)
    end.
    
start_client_request(Req, S, Method, Path, Headers, Url) ->
    #url{host=Host, port=Port} = couchdbproxy_util:parse_url(Url),    
    HProxy = mochiweb_headers:make(Headers),
    HProxy1 = mochiweb_headers:enter("Host",  lists:append([Host, ":", integer_to_list(Port)]), HProxy),
    HProxy2 = mochiweb_headers:default_from_list(proxy_headers(Req),
                                                    HProxy1),
    start_raw_client_request(S, Method, Path, HProxy2).
    
start_raw_client_request(S, Method, Path, Headers) ->
    HStr = headers_to_str(Headers),
    lhttpc_sock:send(S, [?l2b(Method), <<" ">>, ?l2b(Path), <<" HTTP/1.1 ">>, <<"\r\n">> | HStr], false).
                    
headers_to_str(Headers) ->
    F = fun ({K, V}, Acc) ->
                [make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    lists:foldl(F, [<<"\r\n">>], mochiweb_headers:to_list(Headers)).

rewrite_headers(Hdrs, Host) ->
    MHdrs = mochiweb_headers:make(Hdrs),
    mochiweb_headers:to_list(mochiweb_headers:enter("Host", Host, MHdrs)).

%% internal api

convert_method(Method) when is_atom(Method) ->
    atom_to_list(Method);
convert_method(Method) when is_list(Method) ->
    Method.

fix_location([],_) -> [];
fix_location([{"Location", CouchDataPath}|Rest],
             ExternalPath) ->
  	{Scheme, _Netloc, Path, Query, Fragment} = mochiweb_util:urlsplit(CouchDataPath),
	NewPath = mochiweb_util:urlunsplit({Scheme, ExternalPath, Path, Query, Fragment}),
    [{"Location", NewPath}|Rest];
fix_location([H|T], C) ->
    [H|fix_location(T, C)].


write_chunk(Socket, Data) ->
    Length = iolist_size(Data),
    lhttpc_sock:send(Socket, [io_lib:format("~.16b\r\n", [Length]), Data, <<"\r\n">>], false).
    
    
proxy_headers(#proxy{mochi_req=MochiReq}) ->
    Host = couchdbproxy_web:host(MochiReq),
	RemoteAddr = MochiReq:get(peer),
	[{"X-Forwarded-For", RemoteAddr}, 
		{"X-Forwarded-Host", Host}].
				
make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.
