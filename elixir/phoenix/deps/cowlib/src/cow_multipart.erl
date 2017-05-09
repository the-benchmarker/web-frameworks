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

-module(cow_multipart).

%% Parsing.
-export([parse_headers/2]).
-export([parse_body/2]).

%% Building.
-export([boundary/0]).
-export([first_part/2]).
-export([part/2]).
-export([close/1]).

%% Headers.
-export([form_data/1]).
-export([parse_content_disposition/1]).
-export([parse_content_transfer_encoding/1]).
-export([parse_content_type/1]).

-type headers() :: [{iodata(), iodata()}].
-export_type([headers/0]).

-include("cow_inline.hrl").

-define(TEST1_MIME, <<
	"This is a message with multiple parts in MIME format.\r\n"
	"--frontier\r\n"
	"Content-Type: text/plain\r\n"
	"\r\n"
	"This is the body of the message.\r\n"
	"--frontier\r\n"
	"Content-Type: application/octet-stream\r\n"
	"Content-Transfer-Encoding: base64\r\n"
	"\r\n"
	"PGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\n"
	"Ym9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==\r\n"
	"--frontier--"
>>).
-define(TEST1_BOUNDARY, <<"frontier">>).

-define(TEST2_MIME, <<
	"--AaB03x\r\n"
	"Content-Disposition: form-data; name=\"submit-name\"\r\n"
	"\r\n"
	"Larry\r\n"
	"--AaB03x\r\n"
	"Content-Disposition: form-data; name=\"files\"\r\n"
	"Content-Type: multipart/mixed; boundary=BbC04y\r\n"
	"\r\n"
	"--BbC04y\r\n"
	"Content-Disposition: file; filename=\"file1.txt\"\r\n"
	"Content-Type: text/plain\r\n"
	"\r\n"
	"... contents of file1.txt ...\r\n"
	"--BbC04y\r\n"
	"Content-Disposition: file; filename=\"file2.gif\"\r\n"
	"Content-Type: image/gif\r\n"
	"Content-Transfer-Encoding: binary\r\n"
	"\r\n"
	"...contents of file2.gif...\r\n"
	"--BbC04y--\r\n"
	"--AaB03x--"
>>).
-define(TEST2_BOUNDARY, <<"AaB03x">>).

-define(TEST3_MIME, <<
	"This is the preamble.\r\n"
	"--boundary\r\n"
	"Content-Type: text/plain\r\n"
	"\r\n"
	"This is the body of the message.\r\n"
	"--boundary--"
	"\r\nThis is the epilogue. Here it includes leading CRLF"
>>).
-define(TEST3_BOUNDARY, <<"boundary">>).

-define(TEST4_MIME, <<
	"This is the preamble.\r\n"
	"--boundary\r\n"
	"Content-Type: text/plain\r\n"
	"\r\n"
	"This is the body of the message.\r\n"
	"--boundary--"
	"\r\n"
>>).
-define(TEST4_BOUNDARY, <<"boundary">>).

%% Parsing.
%%
%% The multipart format is defined in RFC 2045.

%% @doc Parse the headers for the next multipart part.
%%
%% This function skips any preamble before the boundary.
%% The preamble may be retrieved using parse_body/2.
%%
%% This function will accept input of any size, it is
%% up to the caller to limit it if needed.

-spec parse_headers(binary(), binary())
	-> more | {more, binary()}
	| {ok, headers(), binary()}
	| {done, binary()}.
%% If the stream starts with the boundary we can make a few assumptions
%% and quickly figure out if we got the complete list of headers.
parse_headers(<< "--", Stream/bits >>, Boundary) ->
	BoundarySize = byte_size(Boundary),
	case Stream of
		%% Last boundary. Return the epilogue.
		<< Boundary:BoundarySize/binary, "--", Stream2/bits >> ->
			{done, Stream2};
		<< Boundary:BoundarySize/binary, Stream2/bits >> ->
			%% We have all the headers only if there is a \r\n\r\n
			%% somewhere in the data after the boundary.
			case binary:match(Stream2, <<"\r\n\r\n">>) of
				nomatch ->
					more;
				_ ->
					before_parse_headers(Stream2)
			end;
		%% If there isn't enough to represent Boundary \r\n\r\n
		%% then we definitely don't have all the headers.
		_ when byte_size(Stream) < byte_size(Boundary) + 4 ->
			more;
		%% Otherwise we have preamble data to skip.
		%% We still got rid of the first two misleading bytes.
		_ ->
			skip_preamble(Stream, Boundary)
	end;
%% Otherwise we have preamble data to skip.
parse_headers(Stream, Boundary) ->
	skip_preamble(Stream, Boundary).

%% We need to find the boundary and a \r\n\r\n after that.
%% Since the boundary isn't at the start, it must be right
%% after a \r\n too.
skip_preamble(Stream, Boundary) ->
	case binary:match(Stream, <<"\r\n--", Boundary/bits >>) of
		%% No boundary, need more data.
		nomatch ->
			%% We can safely skip the size of the stream
			%% minus the last 3 bytes which may be a partial boundary.
			SkipSize = byte_size(Stream) - 3,
			case SkipSize > 0 of
				false ->
					more;
				true ->
					<< _:SkipSize/binary, Stream2/bits >> = Stream,
					{more, Stream2}
			end;
		{Start, Length} ->
			Start2 = Start + Length,
			<< _:Start2/binary, Stream2/bits >> = Stream,
			case Stream2 of
				%% Last boundary. Return the epilogue.
				<< "--", Stream3/bits >> ->
					{done, Stream3};
				_ ->
					case binary:match(Stream, <<"\r\n\r\n">>) of
						%% We don't have the full headers.
						nomatch ->
							{more, Stream2};
						_ ->
							before_parse_headers(Stream2)
					end
			end
	end.

%% There is a line break right after the boundary, skip it.
%%
%% We only skip it now because there might be no headers at all,
%% which means the \r\n\r\n indicating the end of headers also
%% includes this line break.
before_parse_headers(<< "\r\n", Stream/bits >>) ->
	parse_hd_name(Stream, [], <<>>).

parse_hd_name(<< C, Rest/bits >>, H, SoFar) ->
	case C of
		$: -> parse_hd_before_value(Rest, H, SoFar);
		$\s -> parse_hd_name_ws(Rest, H, SoFar);
		$\t -> parse_hd_name_ws(Rest, H, SoFar);
		?INLINE_LOWERCASE(parse_hd_name, Rest, H, SoFar)
	end.

parse_hd_name_ws(<< C, Rest/bits >>, H, Name) ->
	case C of
		$\s -> parse_hd_name_ws(Rest, H, Name);
		$\t -> parse_hd_name_ws(Rest, H, Name);
		$: -> parse_hd_before_value(Rest, H, Name)
	end.

parse_hd_before_value(<< $\s, Rest/bits >>, H, N) ->
	parse_hd_before_value(Rest, H, N);
parse_hd_before_value(<< $\t, Rest/bits >>, H, N) ->
	parse_hd_before_value(Rest, H, N);
parse_hd_before_value(Buffer, H, N) ->
	parse_hd_value(Buffer, H, N, <<>>).

parse_hd_value(<< $\r, Rest/bits >>, Headers, Name, SoFar) ->
	case Rest of
		<< "\n\r\n", Rest2/bits >> ->
			{ok, [{Name, SoFar}|Headers], Rest2};
		<< $\n, C, Rest2/bits >> when C =:= $\s; C =:= $\t ->
			parse_hd_value(Rest2, Headers, Name, SoFar);
		<< $\n, Rest2/bits >> ->
			parse_hd_name(Rest2, [{Name, SoFar}|Headers], <<>>)
	end;
parse_hd_value(<< C, Rest/bits >>, H, N, SoFar) ->
	parse_hd_value(Rest, H, N, << SoFar/binary, C >>).

%% @doc Parse the body of the current multipart part.
%%
%% The body is everything until the next boundary.

-spec parse_body(binary(), binary())
	-> {ok, binary()} | {ok, binary(), binary()}
	| done | {done, binary()} | {done, binary(), binary()}.
parse_body(Stream, Boundary) ->
	BoundarySize = byte_size(Boundary),
	case Stream of
		<< "--", Boundary:BoundarySize/binary, _/bits >> ->
			done;
		_ ->
			case binary:match(Stream, << "\r\n--", Boundary/bits >>) of
				%% No boundary, check for a possible partial at the end.
				%% Return more or less of the body depending on the result.
				nomatch ->
					StreamSize = byte_size(Stream),
					From = StreamSize - BoundarySize - 3,
					MatchOpts = if
						%% Binary too small to contain boundary, check it fully.
						From < 0 -> [];
						%% Optimize, only check the end of the binary.
						true -> [{scope, {From, StreamSize - From}}]
					end,
					case binary:match(Stream, <<"\r">>, MatchOpts) of
						nomatch ->
							{ok, Stream};
						{Pos, _} ->
							case Stream of
								<< Body:Pos/binary >> ->
									{ok, Body};
								<< Body:Pos/binary, Rest/bits >> ->
									{ok, Body, Rest}
							end
					end;
				%% Boundary found, this is the last chunk of the body.
				{Pos, _} ->
					case Stream of
						<< Body:Pos/binary, "\r\n" >> ->
							{done, Body};
						<< Body:Pos/binary, "\r\n", Rest/bits >> ->
							{done, Body, Rest};
						<< Body:Pos/binary, Rest/bits >> ->
							{done, Body, Rest}
					end
			end
	end.

-ifdef(TEST).
parse_test() ->
	H1 = [{<<"content-type">>, <<"text/plain">>}],
	Body1 = <<"This is the body of the message.">>,
	H2 = lists:sort([{<<"content-type">>, <<"application/octet-stream">>},
		{<<"content-transfer-encoding">>, <<"base64">>}]),
	Body2 = <<"PGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\n"
		"Ym9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==">>,
	{ok, H1, Rest} = parse_headers(?TEST1_MIME, ?TEST1_BOUNDARY),
	{done, Body1, Rest2} = parse_body(Rest, ?TEST1_BOUNDARY),
	done = parse_body(Rest2, ?TEST1_BOUNDARY),
	{ok, H2Unsorted, Rest3} = parse_headers(Rest2, ?TEST1_BOUNDARY),
	H2 = lists:sort(H2Unsorted),
	{done, Body2, Rest4} = parse_body(Rest3, ?TEST1_BOUNDARY),
	done = parse_body(Rest4, ?TEST1_BOUNDARY),
	{done, <<>>} = parse_headers(Rest4, ?TEST1_BOUNDARY),
	ok.

parse_interleaved_test() ->
	H1 = [{<<"content-disposition">>, <<"form-data; name=\"submit-name\"">>}],
	Body1 = <<"Larry">>,
	H2 = lists:sort([{<<"content-disposition">>, <<"form-data; name=\"files\"">>},
		{<<"content-type">>, <<"multipart/mixed; boundary=BbC04y">>}]),
	InH1 = lists:sort([{<<"content-disposition">>, <<"file; filename=\"file1.txt\"">>},
		{<<"content-type">>, <<"text/plain">>}]),
	InBody1 = <<"... contents of file1.txt ...">>,
	InH2 = lists:sort([{<<"content-disposition">>, <<"file; filename=\"file2.gif\"">>},
		{<<"content-type">>, <<"image/gif">>},
		{<<"content-transfer-encoding">>, <<"binary">>}]),
	InBody2 = <<"...contents of file2.gif...">>,
	{ok, H1, Rest} = parse_headers(?TEST2_MIME, ?TEST2_BOUNDARY),
	{done, Body1, Rest2} = parse_body(Rest, ?TEST2_BOUNDARY),
	done = parse_body(Rest2, ?TEST2_BOUNDARY),
	{ok, H2Unsorted, Rest3} = parse_headers(Rest2, ?TEST2_BOUNDARY),
	H2 = lists:sort(H2Unsorted),
	{_, ContentType} = lists:keyfind(<<"content-type">>, 1, H2),
	{<<"multipart">>, <<"mixed">>, [{<<"boundary">>, InBoundary}]}
		= parse_content_type(ContentType),
	{ok, InH1Unsorted, InRest} = parse_headers(Rest3, InBoundary),
	InH1 = lists:sort(InH1Unsorted),
	{done, InBody1, InRest2} = parse_body(InRest, InBoundary),
	done = parse_body(InRest2, InBoundary),
	{ok, InH2Unsorted, InRest3} = parse_headers(InRest2, InBoundary),
	InH2 = lists:sort(InH2Unsorted),
	{done, InBody2, InRest4} = parse_body(InRest3, InBoundary),
	done = parse_body(InRest4, InBoundary),
	{done, Rest4} = parse_headers(InRest4, InBoundary),
	{done, <<>>} = parse_headers(Rest4, ?TEST2_BOUNDARY),
	ok.

parse_epilogue_test() ->
	H1 = [{<<"content-type">>, <<"text/plain">>}],
	Body1 = <<"This is the body of the message.">>,
	Epilogue = <<"\r\nThis is the epilogue. Here it includes leading CRLF">>,
	{ok, H1, Rest} = parse_headers(?TEST3_MIME, ?TEST3_BOUNDARY),
	{done, Body1, Rest2} = parse_body(Rest, ?TEST3_BOUNDARY),
	done = parse_body(Rest2, ?TEST3_BOUNDARY),
	{done, Epilogue} = parse_headers(Rest2, ?TEST3_BOUNDARY),
	ok.

parse_epilogue_crlf_test() ->
	H1 = [{<<"content-type">>, <<"text/plain">>}],
	Body1 = <<"This is the body of the message.">>,
	Epilogue = <<"\r\n">>,
	{ok, H1, Rest} = parse_headers(?TEST4_MIME, ?TEST4_BOUNDARY),
	{done, Body1, Rest2} = parse_body(Rest, ?TEST4_BOUNDARY),
	done = parse_body(Rest2, ?TEST4_BOUNDARY),
	{done, Epilogue} = parse_headers(Rest2, ?TEST4_BOUNDARY),
	ok.

parse_partial_test() ->
	{ok, <<0:8000, "abcdef">>, <<"\rghij">>}
		= parse_body(<<0:8000, "abcdef\rghij">>, <<"boundary">>),
	{ok, <<"abcdef">>, <<"\rghij">>}
		= parse_body(<<"abcdef\rghij">>, <<"boundary">>),
	{ok, <<"abc">>, <<"\rdef">>}
		= parse_body(<<"abc\rdef">>, <<"boundaryboundary">>),
	{ok, <<0:8000, "abcdef">>, <<"\r\nghij">>}
		= parse_body(<<0:8000, "abcdef\r\nghij">>, <<"boundary">>),
	{ok, <<"abcdef">>, <<"\r\nghij">>}
		= parse_body(<<"abcdef\r\nghij">>, <<"boundary">>),
	{ok, <<"abc">>, <<"\r\ndef">>}
		= parse_body(<<"abc\r\ndef">>, <<"boundaryboundary">>),
	{ok, <<"boundary">>, <<"\r">>}
		= parse_body(<<"boundary\r">>, <<"boundary">>),
	{ok, <<"boundary">>, <<"\r\n">>}
		= parse_body(<<"boundary\r\n">>, <<"boundary">>),
	{ok, <<"boundary">>, <<"\r\n-">>}
		= parse_body(<<"boundary\r\n-">>, <<"boundary">>),
	{ok, <<"boundary">>, <<"\r\n--">>}
		= parse_body(<<"boundary\r\n--">>, <<"boundary">>),
	ok.
-endif.

-ifdef(PERF).
perf_parse_multipart(Stream, Boundary) ->
	case parse_headers(Stream, Boundary) of
		{ok, _, Rest} ->
			{_, _, Rest2} = parse_body(Rest, Boundary),
			perf_parse_multipart(Rest2, Boundary);
		{done, _} ->
			ok
	end.

horse_parse() ->
	horse:repeat(50000,
		perf_parse_multipart(?TEST1_MIME, ?TEST1_BOUNDARY)
	).
-endif.

%% Building.

%% @doc Generate a new random boundary.
%%
%% The boundary generated has a low probability of ever appearing
%% in the data.

-spec boundary() -> binary().
boundary() ->
	base64:encode(crypto:rand_bytes(48)).

%% @doc Return the first part's head.
%%
%% This works exactly like the part/2 function except there is
%% no leading \r\n. It's not required to use this function,
%% just makes the output a little smaller and prettier.

-spec first_part(binary(), headers()) -> iodata().
first_part(Boundary, Headers) ->
	[<<"--">>, Boundary, <<"\r\n">>, headers_to_iolist(Headers, [])].

%% @doc Return a part's head.

-spec part(binary(), headers()) -> iodata().
part(Boundary, Headers) ->
	[<<"\r\n--">>, Boundary, <<"\r\n">>, headers_to_iolist(Headers, [])].

headers_to_iolist([], Acc) ->
	lists:reverse([<<"\r\n">>|Acc]);
headers_to_iolist([{N, V}|Tail], Acc) ->
	%% We don't want to create a sublist so we list the
	%% values in reverse order so that it gets reversed properly.
	headers_to_iolist(Tail, [<<"\r\n">>, V, <<": ">>, N|Acc]).

%% @doc Return the closing delimiter of the multipart message.

-spec close(binary()) -> iodata().
close(Boundary) ->
	[<<"\r\n--">>, Boundary, <<"--">>].

-ifdef(TEST).
build_test() ->
	Result = string:to_lower(binary_to_list(?TEST1_MIME)),
	Result = string:to_lower(binary_to_list(iolist_to_binary([
		<<"This is a message with multiple parts in MIME format.\r\n">>,
		first_part(?TEST1_BOUNDARY, [{<<"content-type">>, <<"text/plain">>}]),
		<<"This is the body of the message.">>,
		part(?TEST1_BOUNDARY, [
			{<<"content-type">>, <<"application/octet-stream">>},
			{<<"content-transfer-encoding">>, <<"base64">>}]),
		<<"PGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\n"
			"Ym9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==">>,
		close(?TEST1_BOUNDARY)
	]))),
	ok.

identity_test() ->
	B = boundary(),
	Preamble = <<"This is a message with multiple parts in MIME format.">>,
	H1 = [{<<"content-type">>, <<"text/plain">>}],
	Body1 = <<"This is the body of the message.">>,
	H2 = lists:sort([{<<"content-type">>, <<"application/octet-stream">>},
		{<<"content-transfer-encoding">>, <<"base64">>}]),
	Body2 = <<"PGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\n"
		"Ym9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==">>,
	Epilogue = <<"Gotta go fast!">>,
	M = iolist_to_binary([
		Preamble,
		part(B, H1), Body1,
		part(B, H2), Body2,
		close(B),
		Epilogue
	]),
	{done, Preamble, M2} = parse_body(M, B),
	{ok, H1, M3} = parse_headers(M2, B),
	{done, Body1, M4} = parse_body(M3, B),
	{ok, H2Unsorted, M5} = parse_headers(M4, B),
	H2 = lists:sort(H2Unsorted),
	{done, Body2, M6} = parse_body(M5, B),
	{done, Epilogue} = parse_headers(M6, B),
	ok.
-endif.

-ifdef(PERF).
perf_build_multipart() ->
	B = boundary(),
	[
		<<"preamble\r\n">>,
		first_part(B, [{<<"content-type">>, <<"text/plain">>}]),
		<<"This is the body of the message.">>,
		part(B, [
			{<<"content-type">>, <<"application/octet-stream">>},
			{<<"content-transfer-encoding">>, <<"base64">>}]),
		<<"PGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\n"
			"Ym9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==">>,
		close(B),
		<<"epilogue">>
	].

horse_build() ->
	horse:repeat(50000,
		perf_build_multipart()
	).
-endif.

%% Headers.

%% @doc Convenience function for extracting information from headers
%% when parsing a multipart/form-data stream.

-spec form_data(headers())
	-> {data, binary()}
	| {file, binary(), binary(), binary(), binary()}.
form_data(Headers) ->
	{_, DispositionBin} = lists:keyfind(<<"content-disposition">>, 1, Headers),
	{<<"form-data">>, Params} = parse_content_disposition(DispositionBin),
	{_, FieldName} = lists:keyfind(<<"name">>, 1, Params),
	case lists:keyfind(<<"filename">>, 1, Params) of
		false ->
			{data, FieldName};
		{_, Filename} ->
			Type = case lists:keyfind(<<"content-type">>, 1, Headers) of
				false -> <<"text/plain">>;
				{_, T} -> T
			end,
			TransferEncoding = case lists:keyfind(
					<<"content-transfer-encoding">>, 1, Headers) of
				false -> <<"7bit">>;
				{_, TE} -> TE
			end,
			{file, FieldName, Filename, Type, TransferEncoding}
	end.

-ifdef(TEST).
form_data_test_() ->
	Tests = [
		{[{<<"content-disposition">>, <<"form-data; name=\"submit-name\"">>}],
			{data, <<"submit-name">>}},
		{[{<<"content-disposition">>,
				<<"form-data; name=\"files\"; filename=\"file1.txt\"">>},
			{<<"content-type">>, <<"text/x-plain">>}],
			{file, <<"files">>, <<"file1.txt">>,
				<<"text/x-plain">>, <<"7bit">>}}
	],
	[{lists:flatten(io_lib:format("~p", [V])),
		fun() -> R = form_data(V) end} || {V, R} <- Tests].
-endif.

%% @todo parse_content_description
%% @todo parse_content_id

%% @doc Parse an RFC 2183 content-disposition value.
%% @todo Support RFC 2231.

-spec parse_content_disposition(binary())
	-> {binary(), [{binary(), binary()}]}.
parse_content_disposition(Bin) ->
	parse_cd_type(Bin, <<>>).

parse_cd_type(<<>>, Acc) ->
	{Acc, []};
parse_cd_type(<< C, Rest/bits >>, Acc) ->
	case C of
		$; -> {Acc, parse_before_param(Rest, [])};
		$\s -> {Acc, parse_before_param(Rest, [])};
		$\t -> {Acc, parse_before_param(Rest, [])};
		?INLINE_LOWERCASE(parse_cd_type, Rest, Acc)
	end.

-ifdef(TEST).
parse_content_disposition_test_() ->
	Tests = [
		{<<"inline">>, {<<"inline">>, []}},
		{<<"attachment">>, {<<"attachment">>, []}},
		{<<"attachment; filename=genome.jpeg;"
			"  modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";">>,
			{<<"attachment">>, [
				{<<"filename">>, <<"genome.jpeg">>},
				{<<"modification-date">>, <<"Wed, 12 Feb 1997 16:29:51 -0500">>}
			]}},
		{<<"form-data; name=\"user\"">>,
			{<<"form-data">>, [{<<"name">>, <<"user">>}]}},
		{<<"form-data; NAME=\"submit-name\"">>,
			{<<"form-data">>, [{<<"name">>, <<"submit-name">>}]}},
		{<<"form-data; name=\"files\"; filename=\"file1.txt\"">>,
			{<<"form-data">>, [
				{<<"name">>, <<"files">>},
				{<<"filename">>, <<"file1.txt">>}
			]}},
		{<<"file; filename=\"file1.txt\"">>,
			{<<"file">>, [{<<"filename">>, <<"file1.txt">>}]}},
		{<<"file; filename=\"file2.gif\"">>,
			{<<"file">>, [{<<"filename">>, <<"file2.gif">>}]}}
	],
	[{V, fun() -> R = parse_content_disposition(V) end} || {V, R} <- Tests].
-endif.

-ifdef(PERF).
horse_parse_content_disposition_attachment() ->
	horse:repeat(100000,
		parse_content_disposition(<<"attachment; filename=genome.jpeg;"
			"  modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";">>)
	).

horse_parse_content_disposition_form_data() ->
	horse:repeat(100000,
		parse_content_disposition(
			<<"form-data; name=\"files\"; filename=\"file1.txt\"">>)
	).

horse_parse_content_disposition_inline() ->
	horse:repeat(100000,
		parse_content_disposition(<<"inline">>)
	).
-endif.

%% @doc Parse an RFC 2045 content-transfer-encoding header.

-spec parse_content_transfer_encoding(binary()) -> binary().
parse_content_transfer_encoding(Bin) ->
	?INLINE_LOWERCASE_BC(Bin).

-ifdef(TEST).
parse_content_transfer_encoding_test_() ->
	Tests = [
		{<<"7bit">>, <<"7bit">>},
		{<<"7BIT">>, <<"7bit">>},
		{<<"8bit">>, <<"8bit">>},
		{<<"binary">>, <<"binary">>},
		{<<"quoted-printable">>, <<"quoted-printable">>},
		{<<"base64">>, <<"base64">>},
		{<<"Base64">>, <<"base64">>},
		{<<"BASE64">>, <<"base64">>},
		{<<"bAsE64">>, <<"base64">>}
	],
	[{V, fun() -> R = parse_content_transfer_encoding(V) end}
		|| {V, R} <- Tests].
-endif.

-ifdef(PERF).
horse_parse_content_transfer_encoding() ->
	horse:repeat(100000,
		parse_content_transfer_encoding(<<"QUOTED-PRINTABLE">>)
	).
-endif.

%% @doc Parse an RFC 2045 content-type header.

-spec parse_content_type(binary())
	-> {binary(), binary(), [{binary(), binary()}]}.
parse_content_type(Bin) ->
	parse_ct_type(Bin, <<>>).

parse_ct_type(<< C, Rest/bits >>, Acc) ->
	case C of
		$/ -> parse_ct_subtype(Rest, Acc, <<>>);
		?INLINE_LOWERCASE(parse_ct_type, Rest, Acc)
	end.

parse_ct_subtype(<<>>, Type, Subtype) when Subtype =/= <<>> ->
	{Type, Subtype, []};
parse_ct_subtype(<< C, Rest/bits >>, Type, Acc) ->
	case C of
		$; -> {Type, Acc, parse_before_param(Rest, [])};
		$\s -> {Type, Acc, parse_before_param(Rest, [])};
		$\t -> {Type, Acc, parse_before_param(Rest, [])};
		?INLINE_LOWERCASE(parse_ct_subtype, Rest, Type, Acc)
	end.

-ifdef(TEST).
parse_content_type_test_() ->
	Tests = [
		{<<"image/gif">>,
			{<<"image">>, <<"gif">>, []}},
		{<<"text/plain">>,
			{<<"text">>, <<"plain">>, []}},
		{<<"text/plain; charset=us-ascii">>,
			{<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}},
		{<<"text/plain; charset=\"us-ascii\"">>,
			{<<"text">>, <<"plain">>, [{<<"charset">>, <<"us-ascii">>}]}},
		{<<"multipart/form-data; boundary=AaB03x">>,
			{<<"multipart">>, <<"form-data">>,
				[{<<"boundary">>, <<"AaB03x">>}]}},
		{<<"multipart/mixed; boundary=BbC04y">>,
			{<<"multipart">>, <<"mixed">>, [{<<"boundary">>, <<"BbC04y">>}]}},
		{<<"multipart/mixed; boundary=--------">>,
			{<<"multipart">>, <<"mixed">>, [{<<"boundary">>, <<"--------">>}]}},
		{<<"application/x-horse; filename=genome.jpeg;"
				"  some-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";"
				"  charset=us-ascii; empty=; number=12345">>,
			{<<"application">>, <<"x-horse">>, [
				{<<"filename">>, <<"genome.jpeg">>},
				{<<"some-date">>, <<"Wed, 12 Feb 1997 16:29:51 -0500">>},
				{<<"charset">>, <<"us-ascii">>},
				{<<"empty">>, <<>>},
				{<<"number">>, <<"12345">>}
			]}}
	],
	[{V, fun() -> R = parse_content_type(V) end}
		|| {V, R} <- Tests].
-endif.

-ifdef(PERF).
horse_parse_content_type_zero() ->
	horse:repeat(100000,
		parse_content_type(<<"text/plain">>)
	).

horse_parse_content_type_one() ->
	horse:repeat(100000,
		parse_content_type(<<"text/plain; charset=\"us-ascii\"">>)
	).

horse_parse_content_type_five() ->
	horse:repeat(100000,
		parse_content_type(<<"application/x-horse; filename=genome.jpeg;"
			"  some-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";"
			"  charset=us-ascii; empty=; number=12345">>)
	).
-endif.

%% @doc Parse RFC 2045 parameters.

parse_before_param(<<>>, Params) ->
	lists:reverse(Params);
parse_before_param(<< C, Rest/bits >>, Params) ->
	case C of
		$; -> parse_before_param(Rest, Params);
		$\s -> parse_before_param(Rest, Params);
		$\t -> parse_before_param(Rest, Params);
		?INLINE_LOWERCASE(parse_param_name, Rest, Params, <<>>)
	end.

parse_param_name(<<>>, Params, Acc) ->
	lists:reverse([{Acc, <<>>}|Params]);
parse_param_name(<< C, Rest/bits >>, Params, Acc) ->
	case C of
		$= -> parse_param_value(Rest, Params, Acc);
		?INLINE_LOWERCASE(parse_param_name, Rest, Params, Acc)
	end.

parse_param_value(<<>>, Params, Name) ->
	lists:reverse([{Name, <<>>}|Params]);
parse_param_value(<< C, Rest/bits >>, Params, Name) ->
	case C of
		$" -> parse_param_quoted_value(Rest, Params, Name, <<>>);
		$; -> parse_before_param(Rest, [{Name, <<>>}|Params]);
		$\s -> parse_before_param(Rest, [{Name, <<>>}|Params]);
		$\t -> parse_before_param(Rest, [{Name, <<>>}|Params]);
		C -> parse_param_value(Rest, Params, Name, << C >>)
	end.

parse_param_value(<<>>, Params, Name, Acc) ->
	lists:reverse([{Name, Acc}|Params]);
parse_param_value(<< C, Rest/bits >>, Params, Name, Acc) ->
	case C of
		$; -> parse_before_param(Rest, [{Name, Acc}|Params]);
		$\s -> parse_before_param(Rest, [{Name, Acc}|Params]);
		$\t -> parse_before_param(Rest, [{Name, Acc}|Params]);
		C -> parse_param_value(Rest, Params, Name, << Acc/binary, C >>)
	end.

%% We expect a final $" so no need to test for <<>>.
parse_param_quoted_value(<< $\\, C, Rest/bits >>, Params, Name, Acc) ->
	parse_param_quoted_value(Rest, Params, Name, << Acc/binary, C >>);
parse_param_quoted_value(<< $", Rest/bits >>, Params, Name, Acc) ->
	parse_before_param(Rest, [{Name, Acc}|Params]);
parse_param_quoted_value(<< C, Rest/bits >>, Params, Name, Acc)
		when C =/= $\r ->
	parse_param_quoted_value(Rest, Params, Name, << Acc/binary, C >>).
