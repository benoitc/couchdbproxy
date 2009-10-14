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

-module(couchdbproxy_api).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchdbproxy.hrl").

-export([http_handler/1]).

http_handler(#proxy{mochi_req=Req}) ->
    Body = couchbeam:json_encode({[
            {couchdbproxy, <<"Welcome">>},
            {version, list_to_binary(io_lib:format("~s/~s", [?QUIP, ?CPVSN]))}
        ]}),
    Req:respond({200, couchdbproxy_http:server_headers(), Body}).