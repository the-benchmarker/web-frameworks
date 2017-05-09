%% Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(cow_http_hd).

-export([parse_connection/1]).
-export([parse_content_length/1]).
-export([parse_transfer_encoding/1]).

-include("cow_inline.hrl").

%% @doc Parse the Connection header.

-spec parse_connection(binary()) -> [binary()].
parse_connection(<<"close">>) ->
	[<<"close">>];
parse_connection(<<"keep-alive">>) ->
	[<<"keep-alive">>];
parse_connection(Connection) ->
	nonempty(token_ci_list(Connection, [])).

-ifdef(TEST).
parse_connection_test_() ->
	Tests = [
		{<<"close">>, [<<"close">>]},
		{<<"ClOsE">>, [<<"close">>]},
		{<<"Keep-Alive">>, [<<"keep-alive">>]},
		{<<"keep-alive, Upgrade">>, [<<"keep-alive">>, <<"upgrade">>]}
	],
	[{V, fun() -> R = parse_connection(V) end} || {V, R} <- Tests].
-endif.

-ifdef(PERF).
horse_parse_connection_close() ->
	horse:repeat(200000,
		parse_connection(<<"close">>)
	).

horse_parse_connection_keepalive() ->
	horse:repeat(200000,
		parse_connection(<<"keep-alive">>)
	).

horse_parse_connection_keepalive_upgrade() ->
	horse:repeat(200000,
		parse_connection(<<"keep-alive, upgrade">>)
	).
-endif.

%% @doc Parse the Content-Length header.
%%
%% The value has at least one digit, and may be followed by whitespace.

-spec parse_content_length(binary()) -> non_neg_integer().
parse_content_length(<< $0 >>) -> 0;
parse_content_length(<< $0, R/bits >>) -> number(R, 0);
parse_content_length(<< $1, R/bits >>) -> number(R, 1);
parse_content_length(<< $2, R/bits >>) -> number(R, 2);
parse_content_length(<< $3, R/bits >>) -> number(R, 3);
parse_content_length(<< $4, R/bits >>) -> number(R, 4);
parse_content_length(<< $5, R/bits >>) -> number(R, 5);
parse_content_length(<< $6, R/bits >>) -> number(R, 6);
parse_content_length(<< $7, R/bits >>) -> number(R, 7);
parse_content_length(<< $8, R/bits >>) -> number(R, 8);
parse_content_length(<< $9, R/bits >>) -> number(R, 9).

-ifdef(TEST).
parse_content_length_test_() ->
	Tests = [
		{<<"0">>, 0},
		{<<"42    ">>, 42},
		{<<"69\t">>, 69},
		{<<"1337">>, 1337},
		{<<"1234567890">>, 1234567890},
		{<<"1234567890     ">>, 1234567890}
	],
	[{V, fun() -> R = parse_content_length(V) end} || {V, R} <- Tests].
-endif.

-ifdef(PERF).
horse_parse_content_length_zero() ->
	horse:repeat(100000,
		parse_content_length(<<"0">>)
	).

horse_parse_content_length_giga() ->
	horse:repeat(100000,
		parse_content_length(<<"1234567890">>)
	).
-endif.

%% @doc Parse the Transfer-Encoding header.
%%
%% @todo Extension parameters.

-spec parse_transfer_encoding(binary()) -> [binary()].
parse_transfer_encoding(<<"chunked">>) ->
	[<<"chunked">>];
parse_transfer_encoding(TransferEncoding) ->
	nonempty(token_ci_list(TransferEncoding, [])).

-ifdef(TEST).
parse_transfer_encoding_test_() ->
	Tests = [
		{<<"a , , , ">>, [<<"a">>]},
		{<<" , , , a">>, [<<"a">>]},
		{<<"a , , b">>, [<<"a">>, <<"b">>]},
		{<<"chunked">>, [<<"chunked">>]},
		{<<"chunked, something">>, [<<"chunked">>, <<"something">>]}
	],
	[{V, fun() -> R = parse_transfer_encoding(V) end} || {V, R} <- Tests].

parse_transfer_encoding_error_test_() ->
	Tests = [
		<<>>,
		<<" ">>,
		<<" , ">>,
		<<",,,">>,
		<<"a b">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_transfer_encoding(V)) end}
		|| V <- Tests].
-endif.

-ifdef(PERF).
horse_parse_transfer_encoding_chunked() ->
	horse:repeat(200000,
		parse_transfer_encoding(<<"chunked">>)
	).

horse_parse_transfer_encoding_custom() ->
	horse:repeat(200000,
		parse_transfer_encoding(<<"chunked, something">>)
	).
-endif.

%% Internal.

%% Only return if the list is not empty.
nonempty(L) when L =/= [] -> L.

%% Parse a number optionally followed by whitespace.
number(<< $0, R/bits >>, Acc) -> number(R, Acc * 10);
number(<< $1, R/bits >>, Acc) -> number(R, Acc * 10 + 1);
number(<< $2, R/bits >>, Acc) -> number(R, Acc * 10 + 2);
number(<< $3, R/bits >>, Acc) -> number(R, Acc * 10 + 3);
number(<< $4, R/bits >>, Acc) -> number(R, Acc * 10 + 4);
number(<< $5, R/bits >>, Acc) -> number(R, Acc * 10 + 5);
number(<< $6, R/bits >>, Acc) -> number(R, Acc * 10 + 6);
number(<< $7, R/bits >>, Acc) -> number(R, Acc * 10 + 7);
number(<< $8, R/bits >>, Acc) -> number(R, Acc * 10 + 8);
number(<< $9, R/bits >>, Acc) -> number(R, Acc * 10 + 9);
number(<< $\s, R/bits >>, Acc) -> ws_end(R), Acc;
number(<< $\t, R/bits >>, Acc) -> ws_end(R), Acc;
number(<<>>, Acc) -> Acc.

ws_end(<< $\s, R/bits >>) -> ws_end(R);
ws_end(<< $\t, R/bits >>) -> ws_end(R);
ws_end(<<>>) -> ok.

%% Parse a list of case insensitive tokens.
token_ci_list(<<>>, Acc) -> lists:reverse(Acc);
token_ci_list(<< $\s, R/bits >>, Acc) -> token_ci_list(R, Acc);
token_ci_list(<< $\t, R/bits >>, Acc) -> token_ci_list(R, Acc);
token_ci_list(<< $,, R/bits >>, Acc) -> token_ci_list(R, Acc);
token_ci_list(<< C, R/bits >>, Acc) ->
	case C of
		?INLINE_LOWERCASE(token_ci_list, R, Acc, <<>>)
	end.

token_ci_list(<<>>, Acc, T) -> lists:reverse([T|Acc]);
token_ci_list(<< $\s, R/bits >>, Acc, T) -> token_ci_list_sep(R, Acc, T);
token_ci_list(<< $\t, R/bits >>, Acc, T) -> token_ci_list_sep(R, Acc, T);
token_ci_list(<< $,, R/bits >>, Acc, T) -> token_ci_list(R, [T|Acc]);
token_ci_list(<< C, R/bits >>, Acc, T) ->
	case C of
		?INLINE_LOWERCASE(token_ci_list, R, Acc, T)
	end.

token_ci_list_sep(<<>>, Acc, T) -> lists:reverse([T|Acc]);
token_ci_list_sep(<< $\s, R/bits >>, Acc, T) -> token_ci_list_sep(R, Acc, T);
token_ci_list_sep(<< $\t, R/bits >>, Acc, T) -> token_ci_list_sep(R, Acc, T);
token_ci_list_sep(<< $,, R/bits >>, Acc, T) -> token_ci_list(R, [T|Acc]).
