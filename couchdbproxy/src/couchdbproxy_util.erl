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
%% Some code from ibrowse project
%% copyright 2004 Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%

-module(couchdbproxy_util).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-export([to_hex/1, parse_url/1]).

-include("couchdbproxy.hrl").


parse_url(Url) ->
    parse_url(Url, get_protocol, #url{abspath=Url}, []).

parse_url([$:, $/, $/ | _], get_protocol, Url, []) ->
    {invalid_uri_1, Url};
parse_url([$:, $/, $/ | T], get_protocol, Url, TmpAcc) ->
    Prot = list_to_atom(lists:reverse(TmpAcc)),
    parse_url(T, get_username, 
	      Url#url{protocol = Prot},
	      []);
parse_url([$/ | T], get_username, Url, TmpAcc) ->
    %% No username/password. No  port number
    Url#url{host = lists:reverse(TmpAcc),
	    port = default_port(Url#url.protocol),
	    path = [$/ | T]};
parse_url([$: | T], get_username, Url, TmpAcc) ->
    %% It is possible that no username/password has been
    %% specified. But we'll continue with the assumption that there is
    %% a username/password. If we encounter a '@' later on, there is a
    %% username/password indeed. If we encounter a '/', it was
    %% actually the hostname
    parse_url(T, get_password, 
	      Url#url{username = lists:reverse(TmpAcc)},
	      []);
parse_url([$@ | T], get_username, Url, TmpAcc) ->
    parse_url(T, get_host, 
	      Url#url{username = lists:reverse(TmpAcc),
		      password = ""},
	      []);
parse_url([$@ | T], get_password, Url, TmpAcc) ->
    parse_url(T, get_host, 
	      Url#url{password = lists:reverse(TmpAcc)},
	      []);
parse_url([$/ | T], get_password, Url, TmpAcc) ->
    %% Ok, what we thought was the username/password was the hostname
    %% and portnumber
    #url{username=User} = Url,
    Port = list_to_integer(lists:reverse(TmpAcc)),
    Url#url{host = User,
	    port = Port,
	    username = undefined,
	    password = undefined,
	    path = [$/ | T]};
parse_url([$: | T], get_host, #url{} = Url, TmpAcc) ->
    parse_url(T, get_port, 
	      Url#url{host = lists:reverse(TmpAcc)},
	      []);
parse_url([$/ | T], get_host, #url{protocol=Prot} = Url, TmpAcc) ->
    Url#url{host = lists:reverse(TmpAcc),
	    port = default_port(Prot),
	    path = [$/ | T]};
parse_url([$/ | T], get_port, #url{protocol=Prot} = Url, TmpAcc) ->
    Port = case TmpAcc of
	       [] ->
		   default_port(Prot);
	       _ ->
		   list_to_integer(lists:reverse(TmpAcc))
	   end,
    Url#url{port = Port, path = [$/ | T]};
parse_url([H | T], State, Url, TmpAcc) ->
    parse_url(T, State, Url, [H | TmpAcc]);
parse_url([], get_host, Url, TmpAcc) when TmpAcc /= [] ->
    Url#url{host = lists:reverse(TmpAcc),
	    port = default_port(Url#url.protocol),
	    path = "/"};
parse_url([], get_username, Url, TmpAcc) when TmpAcc /= [] ->
    Url#url{host = lists:reverse(TmpAcc),
	    port = default_port(Url#url.protocol),
	    path = "/"};
parse_url([], get_port, #url{protocol=Prot} = Url, TmpAcc) ->
    Port = case TmpAcc of
	       [] ->
		   default_port(Prot);
	       _ ->
		   list_to_integer(lists:reverse(TmpAcc))
	   end,
    Url#url{port = Port, 
	    path = "/"};
parse_url([], get_password, Url, TmpAcc) ->
    %% Ok, what we thought was the username/password was the hostname
    %% and portnumber
    #url{username=User} = Url,
    Port = case TmpAcc of
	       [] ->
		   default_port(Url#url.protocol);
	       _ ->
		   list_to_integer(lists:reverse(TmpAcc))
	   end,
    Url#url{host = User,
	    port = Port,
	    username = undefined,
	    password = undefined,
	    path = "/"};
parse_url([], State, Url, TmpAcc) ->
    {invalid_uri_2, State, Url, TmpAcc}.

default_port(http)  -> 80;
default_port(https) -> 443;
default_port(ftp)   -> 21.

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.
