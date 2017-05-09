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

-module(cow_http_te).

%% Identity.
-export([stream_identity/2]).
-export([identity/1]).

%% Chunked.
-export([stream_chunked/2]).
-export([chunk/1]).
-export([last_chunk/0]).

%% The state type is the same for both identity and chunked.
-type state() :: {non_neg_integer(), non_neg_integer()}.

-type decode_ret() :: more
	| {more, Data::binary(), state()}
	| {more, Data::binary(), RemLen::non_neg_integer(), state()}
	| {more, Data::binary(), Rest::binary(), state()}
	| {done, TotalLen::non_neg_integer(), Rest::binary()}
	| {done, Data::binary(), TotalLen::non_neg_integer(), Rest::binary()}.
-export_type([decode_ret/0]).

-ifdef(EXTRA).
dripfeed(<< C, Rest/bits >>, Acc, State, F) ->
	case F(<< Acc/binary, C >>, State) of
		more ->
			dripfeed(Rest, << Acc/binary, C >>, State, F);
		{more, _, State2} ->
			dripfeed(Rest, <<>>, State2, F);
		{more, _, Length, State2} when is_integer(Length) ->
			dripfeed(Rest, <<>>, State2, F);
		{more, _, Acc2, State2} ->
			dripfeed(Rest, Acc2, State2, F);
		{done, _, <<>>} ->
			ok;
		{done, _, _, <<>>} ->
			ok
	end.
-endif.

%% Identity.

%% @doc Decode an identity stream.

-spec stream_identity(Data, State)
	-> {more, Data, Len, State} | {done, Data, Len, Data}
	when Data::binary(), State::state(), Len::non_neg_integer().
stream_identity(Data, {Streamed, Total}) ->
	Streamed2 = Streamed + byte_size(Data),
	if
		Streamed2 < Total ->
			{more, Data, Total - Streamed2, {Streamed2, Total}};
		true ->
			Size = Total - Streamed,
			<< Data2:Size/binary, Rest/bits >> = Data,
			{done, Data2, Total, Rest}
	end.

-spec identity(Data) -> Data when Data::iodata().
identity(Data) ->
	Data.

-ifdef(TEST).
stream_identity_test() ->
	{done, <<>>, 0, <<>>}
		= stream_identity(identity(<<>>), {0, 0}),
	{done, <<"\r\n">>, 2, <<>>}
		= stream_identity(identity(<<"\r\n">>), {0, 2}),
	{done, << 0:80000 >>, 10000, <<>>}
		= stream_identity(identity(<< 0:80000 >>), {0, 10000}),
	ok.

stream_identity_parts_test() ->
	{more, << 0:8000 >>, 1999, S1}
		= stream_identity(<< 0:8000 >>, {0, 2999}),
	{more, << 0:8000 >>, 999, S2}
		= stream_identity(<< 0:8000 >>, S1),
	{done, << 0:7992 >>, 2999, <<>>}
		= stream_identity(<< 0:7992 >>, S2),
	ok.
-endif.

-ifdef(PERF).
%% Using the same data as the chunked one for comparison.

horse_stream_identity() ->
	horse:repeat(10000,
		stream_identity(<<
			"4\r\n"
			"Wiki\r\n"
			"5\r\n"
			"pedia\r\n"
			"e\r\n"
			" in\r\n\r\nchunks.\r\n"
			"0\r\n"
			"\r\n">>, {0, 43})
	).

horse_stream_identity_dripfeed() ->
	horse:repeat(10000,
		dripfeed(<<
			"4\r\n"
			"Wiki\r\n"
			"5\r\n"
			"pedia\r\n"
			"e\r\n"
			" in\r\n\r\nchunks.\r\n"
			"0\r\n"
			"\r\n">>, <<>>, {0, 43}, fun stream_identity/2)
	).
-endif.

%% Chunked.

%% @doc Decode a chunked stream.

-spec stream_chunked(Data, State)
	-> more | {more, Data, State} | {more, Data, Len, State}
	| {more, Data, Data, State}
	| {done, Len, Data} | {done, Data, Len, Data}
	when Data::binary(), State::state(), Len::non_neg_integer().
stream_chunked(Data, State) ->
	stream_chunked(Data, State, <<>>).

%% New chunk.
stream_chunked(Data = << C, _/bits >>, {0, Streamed}, Acc) when C =/= $\r ->
	case chunked_len(Data, Streamed, Acc, 0) of
		{next, Rest, State, Acc2} ->
			stream_chunked(Rest, State, Acc2);
		{more, State, Acc2} ->
			{more, Acc2, Data, State};
		Ret ->
			Ret
	end;
%% Trailing \r\n before next chunk.
stream_chunked(<< "\r\n", Rest/bits >>, {2, Streamed}, Acc) ->
	stream_chunked(Rest, {0, Streamed}, Acc);
%% Trailing \r before next chunk.
stream_chunked(<< "\r" >>, {2, Streamed}, Acc) ->
	{more, Acc, {1, Streamed}};
%% Trailing \n before next chunk.
stream_chunked(<< "\n", Rest/bits >>, {1, Streamed}, Acc) ->
	stream_chunked(Rest, {0, Streamed}, Acc);
%% More data needed.
stream_chunked(<<>>, State = {Rem, _}, Acc) ->
	{more, Acc, Rem, State};
%% Chunk data.
stream_chunked(Data, {Rem, Streamed}, Acc) when Rem > 2 ->
	DataSize = byte_size(Data),
	RemSize = Rem - 2,
	case Data of
		<< Chunk:RemSize/binary, "\r\n", Rest/bits >> ->
			stream_chunked(Rest, {0, Streamed + RemSize}, << Acc/binary, Chunk/binary >>);
		<< Chunk:RemSize/binary, "\r" >> ->
			{more, << Acc/binary, Chunk/binary >>, {1, Streamed + RemSize}};
		%% Everything in Data is part of the chunk.
		_ ->
			Rem2 = Rem - DataSize,
			{more, << Acc/binary, Data/binary >>, Rem2, {Rem2, Streamed + DataSize}}
	end.

chunked_len(<< $0, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16);
chunked_len(<< $1, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 1);
chunked_len(<< $2, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 2);
chunked_len(<< $3, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 3);
chunked_len(<< $4, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 4);
chunked_len(<< $5, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 5);
chunked_len(<< $6, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 6);
chunked_len(<< $7, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 7);
chunked_len(<< $8, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 8);
chunked_len(<< $9, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 9);
chunked_len(<< $A, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 10);
chunked_len(<< $B, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 11);
chunked_len(<< $C, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 12);
chunked_len(<< $D, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 13);
chunked_len(<< $E, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 14);
chunked_len(<< $F, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 15);
chunked_len(<< $a, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 10);
chunked_len(<< $b, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 11);
chunked_len(<< $c, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 12);
chunked_len(<< $d, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 13);
chunked_len(<< $e, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 14);
chunked_len(<< $f, R/bits >>, S, A, Len) -> chunked_len(R, S, A, Len * 16 + 15);
%% Final chunk.
chunked_len(<< "\r\n\r\n", R/bits >>, S, <<>>, 0) -> {done, S, R};
chunked_len(<< "\r\n\r\n", R/bits >>, S, A, 0) -> {done, A, S, R};
chunked_len(_, _, _, 0) -> more;
%% Normal chunk. Add 2 to Len for the trailing \r\n.
chunked_len(<< "\r\n", R/bits >>, S, A, Len) -> {next, R, {Len + 2, S}, A};
chunked_len(<<"\r">>, _, <<>>, _) -> more;
chunked_len(<<"\r">>, S, A, _) -> {more, {0, S}, A};
chunked_len(<<>>, _, <<>>, _) -> more;
chunked_len(<<>>, S, A, _) -> {more, {0, S}, A}.

%% @doc Encode a chunk.

-spec chunk(D) -> D when D::iodata().
chunk(Data) ->
	[integer_to_list(iolist_size(Data), 16), <<"\r\n">>,
		Data, <<"\r\n">>].

%% @doc Encode the last chunk of a chunked stream.

-spec last_chunk() -> << _:40 >>.
last_chunk() ->
	<<"0\r\n\r\n">>.

-ifdef(TEST).
stream_chunked_identity_test() ->
	{done, <<"Wikipedia in\r\n\r\nchunks.">>, 23, <<>>}
		= stream_chunked(iolist_to_binary([
			chunk("Wiki"),
			chunk("pedia"),
			chunk(" in\r\n\r\nchunks."),
			last_chunk()
		]), {0, 0}),
	ok.

stream_chunked_one_pass_test() ->
	{done, 0, <<>>} = stream_chunked(<<"0\r\n\r\n">>, {0, 0}),
	{done, <<"Wikipedia in\r\n\r\nchunks.">>, 23, <<>>}
		= stream_chunked(<<
			"4\r\n"
			"Wiki\r\n"
			"5\r\n"
			"pedia\r\n"
			"e\r\n"
			" in\r\n\r\nchunks.\r\n"
			"0\r\n"
			"\r\n">>, {0, 0}),
	ok.

stream_chunked_n_passes_test() ->
	S0 = {0, 0},
	more = stream_chunked(<<"4\r">>, S0),
	{more, <<>>, 6, S1} = stream_chunked(<<"4\r\n">>, S0),
	{more, <<"Wiki">>, 0, S2} = stream_chunked(<<"Wiki\r\n">>, S1),
	{more, <<"pedia">>, <<"e\r">>, S3} = stream_chunked(<<"5\r\npedia\r\ne\r">>, S2),
	{more, <<" in\r\n\r\nchunks.">>, 2, S4} = stream_chunked(<<"e\r\n in\r\n\r\nchunks.">>, S3),
	{done, 23, <<>>} = stream_chunked(<<"\r\n0\r\n\r\n">>, S4),
	%% A few extra for coverage purposes.
	more = stream_chunked(<<"\n3">>, {1, 0}),
	{more, <<"abc">>, 2, {2, 3}} = stream_chunked(<<"\n3\r\nabc">>, {1, 0}),
	{more, <<"abc">>, {1, 3}} = stream_chunked(<<"3\r\nabc\r">>, {0, 0}),
	{more, <<"abc">>, <<"123">>, {0, 3}} = stream_chunked(<<"3\r\nabc\r\n123">>, {0, 0}),
	ok.

stream_chunked_dripfeed_test() ->
	dripfeed(<<
		"4\r\n"
		"Wiki\r\n"
		"5\r\n"
		"pedia\r\n"
		"e\r\n"
		" in\r\n\r\nchunks.\r\n"
		"0\r\n"
		"\r\n">>, <<>>, {0, 0}, fun stream_chunked/2).

do_body_to_chunks(_, <<>>, Acc) ->
	lists:reverse([<<"0\r\n\r\n">>|Acc]);
do_body_to_chunks(ChunkSize, Body, Acc) ->
	BodySize = byte_size(Body),
	ChunkSize2 = case BodySize < ChunkSize of
		true -> BodySize;
		false -> ChunkSize
	end,
	<< Chunk:ChunkSize2/binary, Rest/binary >> = Body,
	ChunkSizeBin = list_to_binary(integer_to_list(ChunkSize2, 16)),
	do_body_to_chunks(ChunkSize, Rest,
		[<< ChunkSizeBin/binary, "\r\n", Chunk/binary, "\r\n" >>|Acc]).

stream_chunked_dripfeed2_test() ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Body2 = iolist_to_binary(do_body_to_chunks(50, Body, [])),
	dripfeed(Body2, <<>>, {0, 0}, fun stream_chunked/2).

stream_chunked_error_test_() ->
	Tests = [
		{<<>>, undefined},
		{<<"\n\naaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, {2, 0}}
	],
	[{lists:flatten(io_lib:format("value ~p state ~p", [V, S])),
		fun() -> {'EXIT', _} = (catch stream_chunked(V, S)) end}
			|| {V, S} <- Tests].
-endif.

-ifdef(PERF).
horse_stream_chunked() ->
	horse:repeat(10000,
		stream_chunked(<<
			"4\r\n"
			"Wiki\r\n"
			"5\r\n"
			"pedia\r\n"
			"e\r\n"
			" in\r\n\r\nchunks.\r\n"
			"0\r\n"
			"\r\n">>, {0, 0})
	).

horse_stream_chunked_dripfeed() ->
	horse:repeat(10000,
		dripfeed(<<
			"4\r\n"
			"Wiki\r\n"
			"5\r\n"
			"pedia\r\n"
			"e\r\n"
			" in\r\n\r\nchunks.\r\n"
			"0\r\n"
			"\r\n">>, <<>>, {0, 43}, fun stream_chunked/2)
	).
-endif.
