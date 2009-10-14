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
-export([server_headers/0]).
-export([send_response/4,send_error/2, send_error/4, make_io/1]).
-export([start_response/2]).

-include("couchdbproxy.hrl").
-define(MAX_RECV_BODY, (1024*1024)).
-define(IDLE_TIMEOUT, infinity).

send_response(#proxy{mochi_req=MochiReq}, Code, Headers, Body) ->
    MochiReq:respond({Code, Headers ++ server_headers(), Body}).

start_response(Req, {Code, ResponseHeaders}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:default_from_list(server_headers(),
                                                    HResponse),
    Req:start_raw_response({Code, HResponse1}).

	
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
	Body = couchbeam:json_encode({[{"error", ErrorStr}, {"reason", Reason}]}),
	send_response(Req, Code, Headers, Body).

server_headers() ->
    [{"Server", "Mochiweb/1.1 CouchdbProxy/" ++ ?CPVSN ++ " (" ++ ?QUIP ++ ")"}].
    
    
make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.