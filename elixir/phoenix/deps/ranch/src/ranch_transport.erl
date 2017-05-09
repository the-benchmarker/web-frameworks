%% Copyright (c) 2012-2016, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_transport).

-export([sendfile/6]).

-type socket() :: any().
-type opts() :: any().
-type sendfile_opts() :: [{chunk_size, non_neg_integer()}].
-export_type([sendfile_opts/0]).

-callback name() -> atom().
-callback secure() -> boolean().
-callback messages() -> {OK::atom(), Closed::atom(), Error::atom()}.
-callback listen(opts()) -> {ok, socket()} | {error, atom()}.
-callback accept(socket(), timeout())
	-> {ok, socket()} | {error, closed | timeout | atom()}.
-callback accept_ack(socket(), timeout()) -> ok.
-callback connect(string(), inet:port_number(), opts())
	-> {ok, socket()} | {error, atom()}.
-callback connect(string(), inet:port_number(), opts(), timeout())
	-> {ok, socket()} | {error, atom()}.
-callback recv(socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | timeout | atom()}.
-callback send(socket(), iodata()) -> ok | {error, atom()}.
-callback sendfile(socket(), file:name() | file:fd())
	-> {ok, non_neg_integer()} | {error, atom()}.
-callback sendfile(socket(), file:name() | file:fd(), non_neg_integer(),
		non_neg_integer()) -> {ok, non_neg_integer()} | {error, atom()}.
-callback sendfile(socket(), file:name() | file:fd(), non_neg_integer(),
		non_neg_integer(), sendfile_opts())
	-> {ok, non_neg_integer()} | {error, atom()}.
-callback setopts(socket(), opts()) -> ok | {error, atom()}.
-callback controlling_process(socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
-callback peername(socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
-callback sockname(socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
-callback shutdown(socket(), read | write | read_write)
	-> ok | {error, atom()}.
-callback close(socket()) -> ok.

%% A fallback for transports that don't have a native sendfile implementation.
%% Note that the ordering of arguments is different from file:sendfile/5 and
%% that this function accepts either a raw file or a file name.
-spec sendfile(module(), socket(), file:filename_all() | file:fd(),
		non_neg_integer(), non_neg_integer(), sendfile_opts())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Transport, Socket, Filename, Offset, Bytes, Opts)
		when is_list(Filename) orelse is_atom(Filename)
		orelse is_binary(Filename) ->
	ChunkSize = chunk_size(Opts),
	case file:open(Filename, [read, raw, binary]) of
		{ok, RawFile} ->
			_ = case Offset of
				0 ->
					ok;
				_ ->
					{ok, _} = file:position(RawFile, {bof, Offset})
			end,
			try
				sendfile_loop(Transport, Socket, RawFile, Bytes, 0, ChunkSize)
			after
				ok = file:close(RawFile)
			end;
		{error, _Reason} = Error ->
			Error
	end;
sendfile(Transport, Socket, RawFile, Offset, Bytes, Opts) ->
	ChunkSize = chunk_size(Opts),
	Initial2 = case file:position(RawFile, {cur, 0}) of
		{ok, Offset} ->
			Offset;
		{ok, Initial} ->
			{ok, _} = file:position(RawFile, {bof, Offset}),
			Initial
		end,
	case sendfile_loop(Transport, Socket, RawFile, Bytes, 0, ChunkSize) of
		{ok, _Sent} = Result ->
			{ok, _} = file:position(RawFile, {bof, Initial2}),
			Result;
		{error, _Reason} = Error ->
			Error
	end.

-spec chunk_size(sendfile_opts()) -> pos_integer().
chunk_size(Opts) ->
	case lists:keyfind(chunk_size, 1, Opts) of
		{chunk_size, ChunkSize}
				when is_integer(ChunkSize) andalso ChunkSize > 0 ->
			ChunkSize;
		{chunk_size, 0} ->
			16#1FFF;
		false ->
			16#1FFF
	end.

-spec sendfile_loop(module(), socket(), file:fd(), non_neg_integer(),
		non_neg_integer(), pos_integer())
	-> {ok, non_neg_integer()} | {error, term()}.
sendfile_loop(_Transport, _Socket, _RawFile, Sent, Sent, _ChunkSize)
		when Sent =/= 0 ->
	%% All requested data has been read and sent, return number of bytes sent.
	{ok, Sent};
sendfile_loop(Transport, Socket, RawFile, Bytes, Sent, ChunkSize) ->
	ReadSize = read_size(Bytes, Sent, ChunkSize),
	case file:read(RawFile, ReadSize) of
		{ok, IoData} ->
			case Transport:send(Socket, IoData) of
				ok ->
					Sent2 = iolist_size(IoData) + Sent,
					sendfile_loop(Transport, Socket, RawFile, Bytes, Sent2,
						ChunkSize);
				{error, _Reason} = Error ->
					Error
			end;
		eof ->
			{ok, Sent};
		{error, _Reason} = Error ->
			Error
	end.

-spec read_size(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
	non_neg_integer().
read_size(0, _Sent, ChunkSize) ->
	ChunkSize;
read_size(Bytes, Sent, ChunkSize) ->
	min(Bytes - Sent, ChunkSize).
