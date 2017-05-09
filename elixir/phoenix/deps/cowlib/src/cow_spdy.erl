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

-module(cow_spdy).

%% Zstream.
-export([deflate_init/0]).
-export([inflate_init/0]).

%% Parse.
-export([split/1]).
-export([parse/2]).

%% Build.
-export([data/3]).
-export([syn_stream/12]).
-export([syn_reply/6]).
-export([rst_stream/2]).
%% @todo settings
-export([ping/1]).
-export([goaway/2]).
%% @todo headers
%% @todo window_update

-include("cow_spdy.hrl").

%% Zstream.

deflate_init() ->
	Zdef = zlib:open(),
	ok = zlib:deflateInit(Zdef),
	_ = zlib:deflateSetDictionary(Zdef, ?ZDICT),
	Zdef.

inflate_init() ->
	Zinf = zlib:open(),
	ok = zlib:inflateInit(Zinf),
	Zinf.

%% Parse.

split(Data = << _:40, Length:24, _/bits >>)
		when byte_size(Data) >= Length + 8 ->
	Length2 = Length + 8,
	<< Frame:Length2/binary, Rest/bits >> = Data,
	{true, Frame, Rest};
split(_) ->
	false.

parse(<< 0:1, StreamID:31, 0:7, IsFinFlag:1, _:24, Data/bits >>, _) ->
	{data, StreamID, from_flag(IsFinFlag), Data};
parse(<< 1:1, 3:15, 1:16, 0:6, IsUnidirectionalFlag:1, IsFinFlag:1,
		_:25, StreamID:31, _:1, AssocToStreamID:31, Priority:3, _:5,
		0:8, Rest/bits >>, Zinf) ->
	case parse_headers(Rest, Zinf) of
		{ok, Headers, [{<<":host">>, Host}, {<<":method">>, Method},
				{<<":path">>, Path}, {<<":scheme">>, Scheme},
				{<<":version">>, Version}]} ->
			{syn_stream, StreamID, AssocToStreamID, from_flag(IsFinFlag),
				from_flag(IsUnidirectionalFlag), Priority, Method,
				Scheme, Host, Path, Version, Headers};
		_ ->
			{error, badprotocol}
	end;
parse(<< 1:1, 3:15, 2:16, 0:7, IsFinFlag:1, _:25,
		StreamID:31, Rest/bits >>, Zinf) ->
	case parse_headers(Rest, Zinf) of
		{ok, Headers, [{<<":status">>, Status}, {<<":version">>, Version}]} ->
			{syn_reply, StreamID, from_flag(IsFinFlag),
				Status, Version, Headers};
		_ ->
			{error, badprotocol}
	end;
parse(<< 1:1, 3:15, 3:16, 0:8, _:56, StatusCode:32 >>, _)
		when StatusCode =:= 0; StatusCode > 11 ->
	{error, badprotocol};
parse(<< 1:1, 3:15, 3:16, 0:8, _:25, StreamID:31, StatusCode:32 >>, _) ->
	Status = case StatusCode of
		1 -> protocol_error;
		2 -> invalid_stream;
		3 -> refused_stream;
		4 -> unsupported_version;
		5 -> cancel;
		6 -> internal_error;
		7 -> flow_control_error;
		8 -> stream_in_use;
		9 -> stream_already_closed;
		10 -> invalid_credentials;
		11 -> frame_too_large
	end,
	{rst_stream, StreamID, Status};
parse(<< 1:1, 3:15, 4:16, 0:7, ClearSettingsFlag:1, _:24,
		NbEntries:32, Rest/bits >>, _) ->
	try
		Settings = [begin
			Is0 = 0,
			Key = case ID of
				1 -> upload_bandwidth;
				2 -> download_bandwidth;
				3 -> round_trip_time;
				4 -> max_concurrent_streams;
				5 -> current_cwnd;
				6 -> download_retrans_rate;
				7 -> initial_window_size;
				8 -> client_certificate_vector_size
			end,
			{Key, Value, from_flag(PersistFlag), from_flag(WasPersistedFlag)}
		end || << Is0:6, WasPersistedFlag:1, PersistFlag:1,
			ID:24, Value:32 >> <= Rest],
		NbEntries = length(Settings),
		{settings, from_flag(ClearSettingsFlag), Settings}
	catch _:_ ->
		{error, badprotocol}
	end;
parse(<< 1:1, 3:15, 6:16, 0:8, _:24, PingID:32 >>, _) ->
	{ping, PingID};
parse(<< 1:1, 3:15, 7:16, 0:8, _:56, StatusCode:32 >>, _)
		when StatusCode > 2 ->
	{error, badprotocol};
parse(<< 1:1, 3:15, 7:16, 0:8, _:25, LastGoodStreamID:31,
		StatusCode:32 >>, _) ->
	Status = case StatusCode of
		0 -> ok;
		1 -> protocol_error;
		2 -> internal_error
	end,
	{goaway, LastGoodStreamID, Status};
parse(<< 1:1, 3:15, 8:16, 0:7, IsFinFlag:1, _:25, StreamID:31,
		Rest/bits >>, Zinf) ->
	case parse_headers(Rest, Zinf) of
		{ok, Headers, []} ->
			{headers, StreamID, from_flag(IsFinFlag), Headers};
		_ ->
			{error, badprotocol}
	end;
parse(<< 1:1, 3:15, 9:16, 0:8, _:57, 0:31 >>, _) ->
	{error, badprotocol};
parse(<< 1:1, 3:15, 9:16, 0:8, _:25, StreamID:31,
		_:1, DeltaWindowSize:31 >>, _) ->
	{window_update, StreamID, DeltaWindowSize};
parse(_, _) ->
	{error, badprotocol}.

parse_headers(Data, Zinf) ->
	[<< NbHeaders:32, Rest/bits >>] = inflate(Zinf, Data),
	parse_headers(Rest, NbHeaders, [], []).

parse_headers(<<>>, 0, Headers, SpHeaders) ->
	{ok, lists:reverse(Headers), lists:sort(SpHeaders)};
parse_headers(<<>>, _, _, _) ->
	error;
parse_headers(_, 0, _, _) ->
	error;
parse_headers(<< 0:32, _/bits >>, _, _, _) ->
	error;
parse_headers(<< L1:32, Key:L1/binary, L2:32, Value:L2/binary, Rest/bits >>,
		NbHeaders, Acc, SpAcc) ->
	case Key of
		<< $:, _/bits >> ->
			parse_headers(Rest, NbHeaders - 1, Acc,
				lists:keystore(Key, 1, SpAcc, {Key, Value}));
		_ ->
			parse_headers(Rest, NbHeaders - 1, [{Key, Value}|Acc], SpAcc)
	end.

inflate(Zinf, Data) ->
	try
		zlib:inflate(Zinf, Data)
	catch _:_ ->
		ok = zlib:inflateSetDictionary(Zinf, ?ZDICT),
		zlib:inflate(Zinf, <<>>)
	end.

from_flag(0) -> false;
from_flag(1) -> true.

%% Build.

data(StreamID, IsFin, Data) ->
	IsFinFlag = to_flag(IsFin),
	Length = iolist_size(Data),
	[<< 0:1, StreamID:31, 0:7, IsFinFlag:1, Length:24 >>, Data].

syn_stream(Zdef, StreamID, AssocToStreamID, IsFin, IsUnidirectional,
		Priority, Method, Scheme, Host, Path, Version, Headers) ->
	IsFinFlag = to_flag(IsFin),
	IsUnidirectionalFlag = to_flag(IsUnidirectional),
	HeaderBlock = build_headers(Zdef, [
		{<<":method">>, Method},
		{<<":scheme">>, Scheme},
		{<<":host">>, Host},
		{<<":path">>, Path},
		{<<":version">>, Version}
		|Headers]),
	Length = 10 + iolist_size(HeaderBlock),
	[<< 1:1, 3:15, 1:16, 0:6, IsUnidirectionalFlag:1, IsFinFlag:1,
		Length:24, 0:1, StreamID:31, 0:1, AssocToStreamID:31,
		Priority:3, 0:5, 0:8 >>, HeaderBlock].

syn_reply(Zdef, StreamID, IsFin, Status, Version, Headers) ->
	IsFinFlag = to_flag(IsFin),
	HeaderBlock = build_headers(Zdef, [
		{<<":status">>, Status},
		{<<":version">>, Version}
		|Headers]),
	Length = 4 + iolist_size(HeaderBlock),
	[<< 1:1, 3:15, 2:16, 0:7, IsFinFlag:1, Length:24,
		0:1, StreamID:31 >>, HeaderBlock].

rst_stream(StreamID, Status) ->
	StatusCode = case Status of
		protocol_error -> 1;
		invalid_stream -> 2;
		refused_stream -> 3;
		unsupported_version -> 4;
		cancel -> 5;
		internal_error -> 6;
		flow_control_error -> 7;
		stream_in_use -> 8;
		stream_already_closed -> 9;
		invalid_credentials -> 10;
		frame_too_large -> 11
	end,
	<< 1:1, 3:15, 3:16, 0:8, 8:24,
		0:1, StreamID:31, StatusCode:32 >>.

%% @todo settings

ping(PingID) ->
	<< 1:1, 3:15, 6:16, 0:8, 4:24, PingID:32 >>.

goaway(LastGoodStreamID, Status) ->
	StatusCode = case Status of
		ok -> 0;
		protocol_error -> 1;
		internal_error -> 2
	end,
	<< 1:1, 3:15, 7:16, 0:8, 8:24,
		0:1, LastGoodStreamID:31, StatusCode:32 >>.

%% @todo headers
%% @todo window_update

build_headers(Zdef, Headers) ->
	NbHeaders = length(Headers),
	Headers2 = [begin
		L1 = iolist_size(Key),
		L2 = iolist_size(Value),
		[<< L1:32 >>, Key, << L2:32 >>, Value]
	end || {Key, Value} <- Headers],
	zlib:deflate(Zdef, [<< NbHeaders:32 >>, Headers2], full).

to_flag(false) -> 0;
to_flag(true) -> 1.
