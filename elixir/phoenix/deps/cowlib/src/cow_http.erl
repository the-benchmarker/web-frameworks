%% Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(cow_http).

%% @todo parse_request_line
-export([parse_status_line/1]).
-export([parse_headers/1]).

-export([parse_fullhost/1]).
-export([parse_fullpath/1]).
-export([parse_version/1]).

-export([request/4]).
-export([version/1]).

-type version() :: 'HTTP/1.0' | 'HTTP/1.1'.
-type status() :: 100..999.
-type headers() :: [{binary(), iodata()}].

-include("cow_inline.hrl").

%% @doc Parse the status line.

-spec parse_status_line(binary()) -> {version(), status(), binary(), binary()}.
parse_status_line(<< "HTTP/1.1 200 OK\r\n", Rest/bits >>) ->
	{'HTTP/1.1', 200, <<"OK">>, Rest};
parse_status_line(<< "HTTP/1.1 404 Not Found\r\n", Rest/bits >>) ->
	{'HTTP/1.1', 404, <<"Not Found">>, Rest};
parse_status_line(<< "HTTP/1.1 500 Internal Server Error\r\n", Rest/bits >>) ->
	{'HTTP/1.1', 500, <<"Internal Server Error">>, Rest};
parse_status_line(<< "HTTP/1.1 ", Status/bits >>) ->
	parse_status_line(Status, 'HTTP/1.1');
parse_status_line(<< "HTTP/1.0 ", Status/bits >>) ->
	parse_status_line(Status, 'HTTP/1.0').

parse_status_line(<< H, T, U, " ", Rest/bits >>, Version)
		when $0 =< H, H =< $9, $0 =< T, T =< $9, $0 =< U, U =< $9 ->
	Status = (H - $0) * 100 + (T - $0) * 10 + (U - $0),
	{Pos, _} = binary:match(Rest, <<"\r">>),
	<< StatusStr:Pos/binary, "\r\n", Rest2/bits >> = Rest,
	{Version, Status, StatusStr, Rest2}.

-ifdef(TEST).
parse_status_line_test_() ->
	Tests = [
		{<<"HTTP/1.1 200 OK\r\nRest">>,
			{'HTTP/1.1', 200, <<"OK">>, <<"Rest">>}},
		{<<"HTTP/1.0 404 Not Found\r\nRest">>,
			{'HTTP/1.0', 404, <<"Not Found">>, <<"Rest">>}},
		{<<"HTTP/1.1 500 Something very funny here\r\nRest">>,
			{'HTTP/1.1', 500, <<"Something very funny here">>, <<"Rest">>}},
		{<<"HTTP/1.1 200 \r\nRest">>,
			{'HTTP/1.1', 200, <<>>, <<"Rest">>}}
	],
	[{V, fun() -> R = parse_status_line(V) end}
		|| {V, R} <- Tests].

parse_status_line_error_test_() ->
	Tests = [
		<<>>,
		<<"HTTP/1.1">>,
		<<"HTTP/1.1 200\r\n">>,
		<<"HTTP/1.1 200 OK">>,
		<<"HTTP/1.1 200 OK\r">>,
		<<"HTTP/1.1 200 OK\n">>,
		<<"HTTP/0.9 200 OK\r\n">>,
		<<"HTTP/1.1 42 Answer\r\n">>,
		<<"HTTP/1.1 999999999 More than OK\r\n">>,
		<<"content-type: text/plain\r\n">>,
		<<0:80, "\r\n">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_status_line(V)) end}
		|| V <- Tests].
-endif.

-ifdef(PERF).
horse_parse_status_line_200() ->
	horse:repeat(200000,
		parse_status_line(<<"HTTP/1.1 200 OK\r\n">>)
	).

horse_parse_status_line_404() ->
	horse:repeat(200000,
		parse_status_line(<<"HTTP/1.1 404 Not Found\r\n">>)
	).

horse_parse_status_line_500() ->
	horse:repeat(200000,
		parse_status_line(<<"HTTP/1.1 500 Internal Server Error\r\n">>)
	).

horse_parse_status_line_other() ->
	horse:repeat(200000,
		parse_status_line(<<"HTTP/1.1 416 Requested range not satisfiable\r\n">>)
	).
-endif.

%% @doc Parse the list of headers.

-spec parse_headers(binary()) -> {[{binary(), binary()}], binary()}.
parse_headers(Data) ->
	parse_header(Data, []).

parse_header(<< $\r, $\n, Rest/bits >>, Acc) ->
	{lists:reverse(Acc), Rest};
parse_header(Data, Acc) ->
	parse_hd_name(Data, Acc, <<>>).

parse_hd_name(<< C, Rest/bits >>, Acc, SoFar) ->
	case C of
		$: -> parse_hd_before_value(Rest, Acc, SoFar);
		$\s -> parse_hd_name_ws(Rest, Acc, SoFar);
		$\t -> parse_hd_name_ws(Rest, Acc, SoFar);
		?INLINE_LOWERCASE(parse_hd_name, Rest, Acc, SoFar)
	end.

parse_hd_name_ws(<< C, Rest/bits >>, Acc, Name) ->
	case C of
		$: -> parse_hd_before_value(Rest, Acc, Name);
		$\s -> parse_hd_name_ws(Rest, Acc, Name);
		$\t -> parse_hd_name_ws(Rest, Acc, Name)
	end.

parse_hd_before_value(<< $\s, Rest/bits >>, Acc, Name) ->
	parse_hd_before_value(Rest, Acc, Name);
parse_hd_before_value(<< $\t, Rest/bits >>, Acc, Name) ->
	parse_hd_before_value(Rest, Acc, Name);
parse_hd_before_value(Data, Acc, Name) ->
	parse_hd_value(Data, Acc, Name, <<>>).

parse_hd_value(<< $\r, Rest/bits >>, Acc, Name, SoFar) ->
	case Rest of
		<< $\n, C, Rest2/bits >> when C =:= $\s; C =:= $\t ->
			parse_hd_value(Rest2, Acc, Name, << SoFar/binary, C >>);
		<< $\n, Rest2/bits >> ->
			parse_header(Rest2, [{Name, SoFar}|Acc])
	end;
parse_hd_value(<< C, Rest/bits >>, Acc, Name, SoFar) ->
	parse_hd_value(Rest, Acc, Name, << SoFar/binary, C >>).

-ifdef(TEST).
parse_headers_test_() ->
	Tests = [
		{<<"\r\nRest">>,
			{[], <<"Rest">>}},
		{<<"Server: Erlang/R17\r\n"
			"Date: Sun, 23 Feb 2014 09:30:39 GMT\r\n"
			"Multiline-Header: why hello!\r\n"
				" I didn't see you all the way over there!\r\n"
			"Content-Length: 12\r\n"
			"Content-Type: text/plain\r\n"
			"\r\nRest">>,
			{[{<<"server">>, <<"Erlang/R17">>},
				{<<"date">>, <<"Sun, 23 Feb 2014 09:30:39 GMT">>},
				{<<"multiline-header">>,
					<<"why hello! I didn't see you all the way over there!">>},
				{<<"content-length">>, <<"12">>},
				{<<"content-type">>, <<"text/plain">>}],
				<<"Rest">>}}
	],
	[{V, fun() -> R = parse_headers(V) end}
		|| {V, R} <- Tests].

parse_headers_error_test_() ->
	Tests = [
		<<>>,
		<<"\r">>,
		<<"Malformed\r\n\r\n">>,
		<<"content-type: text/plain\r\nMalformed\r\n\r\n">>,
		<<"HTTP/1.1 200 OK\r\n\r\n">>,
		<<0:80, "\r\n\r\n">>,
		<<"content-type: text/plain\r\ncontent-length: 12\r\n">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_headers(V)) end}
		|| V <- Tests].
-endif.

-ifdef(PERF).
horse_parse_headers() ->
	horse:repeat(50000,
		parse_headers(<<"Server: Erlang/R17\r\n"
			"Date: Sun, 23 Feb 2014 09:30:39 GMT\r\n"
			"Multiline-Header: why hello!\r\n"
				" I didn't see you all the way over there!\r\n"
			"Content-Length: 12\r\n"
			"Content-Type: text/plain\r\n"
			"\r\nRest">>)
	).
-endif.

%% @doc Extract host and port from a binary.
%%
%% Because the hostname is case insensitive it is converted
%% to lowercase.

-spec parse_fullhost(binary()) -> {binary(), undefined | non_neg_integer()}.
parse_fullhost(Fullhost) ->
	parse_fullhost(Fullhost, false, <<>>).

parse_fullhost(<< $[, Rest/bits >>, false, <<>>) ->
	parse_fullhost(Rest, true, << $[ >>);
parse_fullhost(<<>>, false, Acc) ->
	{Acc, undefined};
%% @todo Optimize.
parse_fullhost(<< $:, Rest/bits >>, false, Acc) ->
	{Acc, list_to_integer(binary_to_list(Rest))};
parse_fullhost(<< $], Rest/bits >>, true, Acc) ->
	parse_fullhost(Rest, false, << Acc/binary, $] >>);
parse_fullhost(<< C, Rest/bits >>, E, Acc) ->
	case C of
		?INLINE_LOWERCASE(parse_fullhost, Rest, E, Acc)
	end.

-ifdef(TEST).
parse_fullhost_test() ->
	{<<"example.org">>, 8080} = parse_fullhost(<<"example.org:8080">>),
	{<<"example.org">>, undefined} = parse_fullhost(<<"example.org">>),
	{<<"192.0.2.1">>, 8080} = parse_fullhost(<<"192.0.2.1:8080">>),
	{<<"192.0.2.1">>, undefined} = parse_fullhost(<<"192.0.2.1">>),
	{<<"[2001:db8::1]">>, 8080} = parse_fullhost(<<"[2001:db8::1]:8080">>),
	{<<"[2001:db8::1]">>, undefined} = parse_fullhost(<<"[2001:db8::1]">>),
	{<<"[::ffff:192.0.2.1]">>, 8080}
		= parse_fullhost(<<"[::ffff:192.0.2.1]:8080">>),
	{<<"[::ffff:192.0.2.1]">>, undefined}
		= parse_fullhost(<<"[::ffff:192.0.2.1]">>),
	ok.
-endif.

%% @doc Extract path and query string from a binary.

-spec parse_fullpath(binary()) -> {binary(), binary()}.
parse_fullpath(Fullpath) ->
	parse_fullpath(Fullpath, <<>>).

parse_fullpath(<<>>, Path) ->
	{Path, <<>>};
parse_fullpath(<< $?, Qs/binary >>, Path) ->
	{Path, Qs};
parse_fullpath(<< C, Rest/binary >>, SoFar) ->
	parse_fullpath(Rest, << SoFar/binary, C >>).

-ifdef(TEST).
parse_fullpath_test() ->
	{<<"*">>, <<>>} = parse_fullpath(<<"*">>),
	{<<"/">>, <<>>} = parse_fullpath(<<"/">>),
	{<<"/path/to/resource">>, <<>>} = parse_fullpath(<<"/path/to/resource">>),
	{<<"/">>, <<>>} = parse_fullpath(<<"/?">>),
	{<<"/">>, <<"q=cowboy">>} = parse_fullpath(<<"/?q=cowboy">>),
	{<<"/path/to/resource">>, <<"q=cowboy">>}
		= parse_fullpath(<<"/path/to/resource?q=cowboy">>),
	ok.
-endif.

%% @doc Convert an HTTP version to atom.

-spec parse_version(binary()) -> version().
parse_version(<<"HTTP/1.1">>) -> 'HTTP/1.1';
parse_version(<<"HTTP/1.0">>) -> 'HTTP/1.0'.

-ifdef(TEST).
parse_version_test() ->
	'HTTP/1.1' = parse_version(<<"HTTP/1.1">>),
	'HTTP/1.0' = parse_version(<<"HTTP/1.0">>),
	{'EXIT', _} = (catch parse_version(<<"HTTP/1.2">>)),
	ok.
-endif.

%% @doc Return formatted request-line and headers.
%% @todo Add tests when the corresponding reverse functions are added.

-spec request(binary(), iodata(), version(), headers()) -> iodata().
request(Method, Path, Version, Headers) ->
	[Method, <<" ">>, Path, <<" ">>, version(Version), <<"\r\n">>,
		[[N, <<": ">>, V, <<"\r\n">>] || {N, V} <- Headers],
		<<"\r\n">>].

%% @doc Return the version as a binary.

-spec version(version()) -> binary().
version('HTTP/1.1') -> <<"HTTP/1.1">>;
version('HTTP/1.0') -> <<"HTTP/1.0">>.

-ifdef(TEST).
version_test() ->
	<<"HTTP/1.1">> = version('HTTP/1.1'),
	<<"HTTP/1.0">> = version('HTTP/1.0'),
	{'EXIT', _} = (catch version('HTTP/1.2')),
	ok.
-endif.
