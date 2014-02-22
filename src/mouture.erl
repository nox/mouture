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

-module(mouture).

-export([parse/1,unparse/1]).
-export([compare/2]).

-export_type([version/0]).

-opaque version() :: {core(),pre(),meta()}.

-type   core()    :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.
-type   pre()     :: [binary() | non_neg_integer()].
-type   meta()    :: [binary()].

%% IO

-spec parse(binary()) -> version().
parse(Bin) ->
  juxta(Bin, [fun core/2, fun preversion/2, fun metadata/2],
        fun (<<>>, [Core,Pre,Meta]) -> {Core,Pre,Meta} end).

-spec unparse(version()) -> iolist().
unparse({{X,Y,Z},Pre,Meta}) ->
    [integer_to_binary(X),$.,integer_to_binary(Y),$.,integer_to_binary(Z),
     unparse_ext($-, fun unparse_pre_seg/1, Pre)
    |unparse_ext($+, fun unparse_meta_seg/1, Meta)].

%% Comparison

-spec compare(version(), version()) -> lt | eq | gt.
compare({Core,Pre,_}, {Core,Pre,_}) ->
    eq;
compare({Core,[],_}, {Core,_,_}) ->
    gt;
compare({Core,_,_}, {Core,[],_}) ->
    lt;
compare({Core,PreA,_}, {Core,PreB,_}) when PreA < PreB ->
    lt;
compare({CoreA,_,_}, {CoreB,_,_}) when CoreA < CoreB ->
    lt;
compare({_,_,_}, {_,_,_}) ->
    gt.

%% Main parts

-spec core(binary(), cont(core(), A)) -> A.
core(Bin, Cont) ->
    dot(Bin, fun integer/2, fun (Rest, [X,Y,Z]) -> Cont(Rest, {X,Y,Z}) end).

-spec preversion(binary(), cont(pre(), A)) -> A.
preversion(<<$-,Rest/binary>>, Cont) ->
    dot(Rest, fun ident_or_integer/2, Cont);
preversion(Bin, Cont) ->
    Cont(Bin, []).

-spec metadata(binary(), cont(meta(), A)) -> A.
metadata(<<$+,Rest/binary>>, Cont) ->
    dot(Rest, fun ident/2, Cont);
metadata(Bin, Cont) ->
    Cont(Bin, []).

%% Base parsers

-type cont(A, B) :: fun((binary(), A) -> B).

-spec ident_or_integer(binary(), cont(binary() | non_neg_integer(), A)) -> A.
ident_or_integer(Bin, Cont) ->
    ident(Bin, fun (Rest, <<$0>>) ->
                       Cont(Rest, 0);
                   (Rest, <<C,_/binary>>=Ident) when C >= $0, C =< $9 ->
                       Cont(Rest, try binary_to_integer(Ident) of
                                      Int when Int =/= 0; C =/= $0 -> Int
                                  catch error:badarg -> Ident end);
                   (Rest, Ident) ->
                       Cont(Rest, Ident)
               end).

-spec ident(binary(), cont(binary(), A)) -> A.
ident(<<C,Rest/binary>>, Cont) when C >= $0, C =< $9; C >= $a, C =< $z;
                                    C >= $A, C =< $Z; C =:= $- ->
    ident(Rest, <<C>>, Cont).

ident(<<C,Rest/binary>>, Ident, Cont) when C >= $0, C =< $9; C >= $a, C =< $z;
                                           C >= $A, C =< $Z; C =:= $- ->
    ident(Rest, <<Ident/binary,C>>, Cont);
ident(Bin, Ident, Cont) ->
    Cont(Bin, Ident).

-spec integer(binary(), cont(non_neg_integer(), A)) -> A.
integer(<<$0,Rest/binary>>, Cont) ->
    Cont(Rest, 0);
integer(<<C,Rest/binary>>, Cont) when C >= $1, C =< $9 ->
    integer(Rest, C - $0, Cont).

integer(<<C,Rest/binary>>, Acc, Cont) when C >= $0, C =< $9 ->
    integer(Rest, Acc * 10 + C - $0, Cont);
integer(Bin, Acc, Cont) ->
    Cont(Bin, Acc).

%% Combinators

-spec juxta(binary(), [fun((binary(), cont(_, A)) -> A)], cont([A], B)) -> B.
juxta(Bin, Fs, Cont) ->
    juxta(Bin, [], Cont, Fs).

juxta(Bin, Acc, Cont, [F|Fs]) ->
    F(Bin, fun (Rest, X) -> juxta(Rest, [X|Acc], Cont, Fs) end);
juxta(Bin, Acc, Cont, []) ->
    Cont(Bin, lists:reverse(Acc)).

-spec dot(binary(), fun((binary(), cont(_, A)) -> A), cont([A], B)) -> B.
dot(Bin, F, Cont) ->
    F(Bin, fun (Rest, X) -> dot(Rest, [X], F, Cont) end).

dot(<<$.,Rest0/binary>>, Acc, F, Cont) ->
    F(Rest0, fun (Rest1, X) -> dot(Rest1, [X|Acc], F, Cont) end);
dot(Bin, Acc, _, Cont) ->
    Cont(Bin, lists:reverse(Acc)).

%% Extensions' unparser

-spec unparse_ext(byte(), fun((A) -> binary()), [A]) -> iolist().
unparse_ext(_, _, []) ->
    [];
unparse_ext(Prefix, F, [H|Rest]) ->
    [Prefix,F(H)|[ [$.,F(Seg)] || Seg <- Rest ]].

-spec unparse_pre_seg(binary() | non_neg_integer()) -> binary().
unparse_pre_seg(Seg) when is_binary(Seg) ->
    Seg;
unparse_pre_seg(Seg) when is_integer(Seg), Seg >= 0 ->
    integer_to_binary(Seg).

-spec unparse_meta_seg(binary()) -> binary().
unparse_meta_seg(Seg) when is_binary(Seg) ->
    Seg.
