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

-module(couchdbproxy_http).
-export([send_chunk/2, start_chunked_response/3, server_headers/0]).
-export([send_response/4, send_redirect/2, send_error/2, send_error/4, make_io/1]).
-export([start_response_chunked/2, start_response/2]).

-include("couchdbproxy.hrl").
-define(MAX_RECV_BODY, (1024*1024)).
-define(IDLE_TIMEOUT, infinity).

start_chunked_response(#proxy{mochi_req=MochiReq}, Code, Headers) ->
    {ok, MochiReq:respond({Code, Headers ++ server_headers(), chunked})}.


send_chunk(Resp, Data) ->
    Resp:write_chunk(Data),
    {ok, Resp}.

send_response(#proxy{mochi_req=MochiReq}, Code, Headers, Body) ->
    if Code >= 400 ->
        io:format("httpd ~p error response:~n ~s", [Code, Body]);
    true -> ok
    end,
    {ok, MochiReq:respond({Code, Headers ++ server_headers(), Body})}.
    

start_response_chunked(MochiReq, {#http_response{version=Version}=HttpResponse, ResponseHeaders}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = case MochiReq:get(method) of
                     'HEAD' ->
                         %% This is what Google does, http://www.google.com/
                         %% is chunked but HEAD gets Content-Length: 0.
                         %% The RFC is ambiguous so emulating Google is smart.
                         mochiweb_headers:enter("Content-Length", "0",
                                                HResponse);
                     _ when Version >= {1, 1} ->
                         %% Only use chunked encoding for HTTP/1.1
                         mochiweb_headers:enter("Transfer-Encoding", "chunked",
                                                HResponse);
                     _ ->
                         %% For pre-1.1 clients we send the data as-is
                         %% without a Content-Length header and without
                         %% chunk delimiters. Since the end of the document
                         %% is now ambiguous we must force a close.
                         erlang:put(mochiweb_request_force_close, true),
                         HResponse
                 end,
    start_response(MochiReq, {HttpResponse, HResponse1}).

start_response(MochiReq, {HttpResponse, ResponseHeaders}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:default_from_list(server_headers(),
                                                    HResponse),
    start_raw_response(MochiReq, {HttpResponse, HResponse1}).


start_raw_response(MochiReq, {#http_response{version=Version,status=Code,
                            phrase=Phrase}, ResponseHeaders}) ->
    F = fun ({K, V}, Acc) ->
                [make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    End = lists:foldl(F, [<<"\r\n">>],
                      mochiweb_headers:to_list(ResponseHeaders)),
    CodePhrase = [integer_to_list(Code), [" " | Phrase]],                  
                      
    MochiReq:send([make_version(Version), CodePhrase, <<"\r\n">> | End]),
    mochiweb:new_response({MochiReq, Code, ResponseHeaders}).




send_redirect(Req, Path) ->
     Headers = [{"Location", Path}],
     send_response(Req, 301, Headers, <<>>).
	
error_info({Error, Reason}) when is_list(Reason) ->
    error_info({Error, ?l2b(Reason)});
error_info(bad_request) ->
    {400, <<"bad_request">>, <<>>};
error_info({bad_request, Reason}) ->
    {400, <<"bad_request">>, Reason};
error_info(not_found) ->
    {404, <<"not_found">>, <<"missing">>};
error_info({not_found, Reason}) ->
    {404, <<"not_found">>, Reason};
error_info({forbidden, Msg}) ->
    {403, <<"forbidden">>, Msg};
error_info({unauthorized, Msg}) ->
    {401, <<"unauthorized">>, Msg};
error_info(bad_gateway) ->
    {502, <<"Bad Gateway">>, <<>>};
error_info({bad_gateway, Reason}) ->
    {502, <<"Bad Gateway">>, Reason};
error_info({Error, Reason}) ->
    {500, couch_util:to_binary(Error), couch_util:to_binary(Reason)};
error_info(Error) ->
    {500, <<"unknown_error">>, couch_util:to_binary(Error)}.

send_error(Req, Error) ->
	{Code, ErrorStr, Reason } = error_info(Error),
	send_error(Req, Code, ErrorStr, Reason).

send_error(Req, Code, ErrorStr, Reason) ->
	Headers =  [{"Content-Type", "text/plain"}],
	Body = io_lib:format("<html><head><title>~s</title></head>"
						"<body><h1>~s</h1> ~p ~n</body></html>", 		
						[ErrorStr, ErrorStr, Reason]),
	send_response(Req, Code, Headers, Body).

server_headers() ->
    [{"Server", "Mochiweb/1.1 CouchdbProxy/" ++ ?CPVSN ++ " (" ++ ?QUIP ++ ")"}].
    
    
make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.