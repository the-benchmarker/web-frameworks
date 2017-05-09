%% Copyright (c) 2011-2016, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_tcp).
-behaviour(ranch_transport).

-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
-export([disallowed_listen_options/0]).
-export([accept/2]).
-export([accept_ack/2]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).

-type opt() :: {backlog, non_neg_integer()}
	| {buffer, non_neg_integer()}
	| {delay_send, boolean()}
	| {dontroute, boolean()}
	| {exit_on_close, boolean()}
	| {fd, non_neg_integer()}
	| {high_msgq_watermark, non_neg_integer()}
	| {high_watermark, non_neg_integer()}
	| inet
	| inet6
	| {ip, inet:ip_address()}
	| {ipv6_v6only, boolean()}
	| {keepalive, boolean()}
	| {linger, {boolean(), non_neg_integer()}}
	| {low_msgq_watermark, non_neg_integer()}
	| {low_watermark, non_neg_integer()}
	| {nodelay, boolean()}
	| {port, inet:port_number()}
	| {priority, integer()}
	| {raw, non_neg_integer(), non_neg_integer(), binary()}
	| {recbuf, non_neg_integer()}
	| {send_timeout, timeout()}
	| {send_timeout_close, boolean()}
	| {sndbuf, non_neg_integer()}
	| {tos, integer()}.
-export_type([opt/0]).

-type opts() :: [opt()].
-export_type([opts/0]).

name() -> tcp.

-spec secure() -> boolean().
secure() ->
    false.

messages() -> {tcp, tcp_closed, tcp_error}.

-spec listen(opts()) -> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	Opts3 = ranch:set_option_default(Opts2, nodelay, true),
	Opts4 = ranch:set_option_default(Opts3, send_timeout, 30000),
	Opts5 = ranch:set_option_default(Opts4, send_timeout_close, true),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	gen_tcp:listen(0, ranch:filter_options(Opts5, disallowed_listen_options(),
		[binary, {active, false}, {packet, raw}, {reuseaddr, true}])).

%% 'binary' and 'list' are disallowed but they are handled
%% specifically as they do not have 2-tuple equivalents.
disallowed_listen_options() ->
	[active, header, mode, packet, packet_size, line_delimiter, reuseaddr].

-spec accept(inet:socket(), timeout())
	-> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	gen_tcp:accept(LSocket, Timeout).

-spec accept_ack(inet:socket(), timeout()) -> ok.
accept_ack(_, _) ->
	ok.

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	gen_tcp:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}]).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	gen_tcp:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}],
		Timeout).

-spec recv(inet:socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout).

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

-spec sendfile(inet:socket(), file:name_all() | file:fd())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) ->
	sendfile(Socket, Filename, 0, 0, []).

-spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
	sendfile(Socket, File, Offset, Bytes, []).

-spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer(), [{chunk_size, non_neg_integer()}])
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename, Offset, Bytes, Opts)
		when is_list(Filename) orelse is_atom(Filename)
		orelse is_binary(Filename) ->
	case file:open(Filename, [read, raw, binary]) of
		{ok, RawFile} ->
			try sendfile(Socket, RawFile, Offset, Bytes, Opts) of
				Result -> Result
			after
				ok = file:close(RawFile)
			end;
		{error, _} = Error ->
			Error
	end;
sendfile(Socket, RawFile, Offset, Bytes, Opts) ->
	Opts2 = case Opts of
		[] -> [{chunk_size, 16#1FFF}];
		_ -> Opts
	end,
	try file:sendfile(RawFile, Socket, Offset, Bytes, Opts2) of
		Result -> Result
	catch
		error:{badmatch, {error, enotconn}} ->
			%% file:sendfile/5 might fail by throwing a
			%% {badmatch, {error, enotconn}}. This is because its
			%% implementation fails with a badmatch in
			%% prim_file:sendfile/10 if the socket is not connected.
			{error, closed}
	end.

%% @todo Probably filter Opts?
-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	inet:setopts(Socket, Opts).

-spec controlling_process(inet:socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	gen_tcp:controlling_process(Socket, Pid).

-spec peername(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	inet:peername(Socket).

-spec sockname(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	inet:sockname(Socket).

-spec shutdown(inet:socket(), read | write | read_write)
	-> ok | {error, atom()}.
shutdown(Socket, How) ->
	gen_tcp:shutdown(Socket, How).

-spec close(inet:socket()) -> ok.
close(Socket) ->
	gen_tcp:close(Socket).
