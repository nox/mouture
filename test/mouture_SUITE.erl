%% Copyright (c) 2014, Anthony Ramine <n.oxyde@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mouture_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([parse/1,unparse/1,compare/1,is_compatible/1]).

all() -> [parse,unparse,compare,is_compatible].

%% IO

parse(Config) when is_list(Config) ->
    {{0,0,0},[],[]} = mouture:parse(<<"0.0.0">>),
    {{1,23,456},[],[]} = mouture:parse(<<"1.23.456">>),
    {{1,0,0},[0],[]} = mouture:parse(<<"1.0.0-0">>),
    {{1,0,0},[<<"0foo">>],[]} = mouture:parse(<<"1.0.0-0foo">>),
    {{1,0,0},[<<"1foo">>],[]} = mouture:parse(<<"1.0.0-1foo">>),
    {{1,0,0},[<<"alpha">>],[]} = mouture:parse(<<"1.0.0-alpha">>),
    {{1,0,0},[1,23,<<"foo">>,4],[]} = mouture:parse(<<"1.0.0-1.23.foo.4">>),
    {{1,0,0},[],[<<"012">>,<<"foo">>]} = mouture:parse(<<"1.0.0+012.foo">>),
    {{1,0,0},[1,2],[<<"3">>,<<"4">>]} = mouture:parse(<<"1.0.0-1.2+3.4">>),
    {'EXIT',_} = (catch mouture:parse(<<"">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0.">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0.0.0">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0..0.0">>)),
    {'EXIT',_} = (catch mouture:parse(<<"00.0.0">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0.0-00">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0.0-00">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0.0-0.">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0.0-0..0">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0.0+">>)),
    {'EXIT',_} = (catch mouture:parse(<<"0.0.0+foo.">>)),
    ok.

unparse(Config) when is_list(Config) ->
    <<"0.0.0">> = mouture:unparse({{0,0,0},[],[]}),
    <<"1.23.456">> = mouture:unparse({{1,23,456},[],[]}),
    <<"1.0.0-0">> = mouture:unparse({{1,0,0},[0],[]}),
    <<"1.0.0-0foo">> = mouture:unparse({{1,0,0},[<<"0foo">>],[]}),
    <<"1.0.0-1foo">> = mouture:unparse({{1,0,0},[<<"1foo">>],[]}),
    <<"1.0.0-alpha">> = mouture:unparse({{1,0,0},[<<"alpha">>],[]}),
    <<"1.0.0-1.23.foo.4">> = mouture:unparse({{1,0,0},[1,23,<<"foo">>,4],[]}),
    <<"1.0.0+12.foo">> = mouture:unparse({{1,0,0},[],[<<"12">>,<<"foo">>]}),
    ok.

%% Comparison and compatibility

compare(Config) when is_list(Config) ->
    gt = compare(<<"1.0.1">>, <<"1.0.0">>),
    gt = compare(<<"1.1.0">>, <<"1.0.1">>),
    gt = compare(<<"2.1.1">>, <<"1.2.2">>),
    gt = compare(<<"1.0.0">>, <<"1.0.0-dev">>),
    gt = compare(<<"1.2.3-dev">>, <<"0.1.2">>),
    gt = compare(<<"1.0.0-a.b">>, <<"1.0.0-a">>),
    gt = compare(<<"1.0.0-b">>, <<"1.0.0-a.b">>),
    gt = compare(<<"1.0.0-a">>, <<"1.0.0-0">>),
    gt = compare(<<"1.0.0-a.b">>, <<"1.0.0-a.a">>),
    lt = compare(<<"1.0.0">>, <<"1.0.1">>),
    lt = compare(<<"1.0.1">>, <<"1.1.0">>),
    lt = compare(<<"1.2.2">>, <<"2.1.1">>),
    lt = compare(<<"1.0.0-dev">>, <<"1.0.0">>),
    lt = compare(<<"0.1.2">>, <<"1.2.3-dev">>),
    lt = compare(<<"1.0.0-a">>, <<"1.0.0-a.b">>),
    lt = compare(<<"1.0.0-a.b">>, <<"1.0.0-b">>),
    lt = compare(<<"1.0.0-0">>, <<"1.0.0-a">>),
    lt = compare(<<"1.0.0-a.a">>, <<"1.0.0-a.b">>),
    eq = compare(<<"1.0.0">>, <<"1.0.0">>),
    eq = compare(<<"1.0.0-dev">>, <<"1.0.0-dev">>),
    eq = compare(<<"1.0.0-a">>, <<"1.0.0-a">>),
    eq = compare(<<"1.0.0+foo">>, <<"1.0.0+bar">>),
    ok.

is_compatible(Config) when is_list(Config) ->
    false = is_compatible(<<"1.0.1">>, <<"1.0.0">>),
    false = is_compatible(<<"1.1.0">>, <<"1.0.1">>),
    false = is_compatible(<<"2.1.1">>, <<"1.2.2">>),
    false = is_compatible(<<"1.0.0">>, <<"1.0.0-dev">>),
    false = is_compatible(<<"1.2.3-dev">>, <<"0.1.2">>),
    false = is_compatible(<<"1.0.0-a.b">>, <<"1.0.0-a">>),
    false = is_compatible(<<"1.0.0-b">>, <<"1.0.0-a.b">>),
    false = is_compatible(<<"1.0.0-a">>, <<"1.0.0-0">>),
    false = is_compatible(<<"1.0.0-a.b">>, <<"1.0.0-a.a">>),
    true  = is_compatible(<<"1.0.0">>, <<"1.0.1">>),
    true  = is_compatible(<<"1.0.1">>, <<"1.1.0">>),
    false = is_compatible(<<"1.2.2">>, <<"2.1.1">>),
    true  = is_compatible(<<"1.0.0-dev">>, <<"1.0.0">>),
    false = is_compatible(<<"0.1.2">>, <<"1.2.3-dev">>),
    true  = is_compatible(<<"1.0.0-a">>, <<"1.0.0-a.b">>),
    true  = is_compatible(<<"1.0.0-a.b">>, <<"1.0.0-b">>),
    true  = is_compatible(<<"1.0.0-0">>, <<"1.0.0-a">>),
    true  = is_compatible(<<"1.0.0-a.a">>, <<"1.0.0-a.b">>),
    true  = is_compatible(<<"1.0.0">>, <<"1.0.0">>),
    true  = is_compatible(<<"1.0.0-dev">>, <<"1.0.0-dev">>),
    true  = is_compatible(<<"1.0.0-a">>, <<"1.0.0-a">>),
    true  = is_compatible(<<"1.0.0+foo">>, <<"1.0.0+bar">>),
    ok.

%% Helpers

compare(V1, V2) ->
    mouture:compare(mouture:parse(V1), mouture:parse(V2)).

is_compatible(V1, V2) ->
    mouture:is_compatible(mouture:parse(V1), mouture:parse(V2)).