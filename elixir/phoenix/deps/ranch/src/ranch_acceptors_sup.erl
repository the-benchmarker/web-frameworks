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

-module(ranch_acceptors_sup).
-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-spec start_link(ranch:ref(), non_neg_integer(), module(), any())
	-> {ok, pid()}.
start_link(Ref, NumAcceptors, Transport, TransOpts) ->
	supervisor:start_link(?MODULE, [Ref, NumAcceptors, Transport, TransOpts]).

init([Ref, NumAcceptors, Transport, TransOpts]) ->
	ConnsSup = ranch_server:get_connections_sup(Ref),
	LSocket = case proplists:get_value(socket, TransOpts) of
		undefined ->
			TransOpts2 = proplists:delete(ack_timeout,
				proplists:delete(connection_type,
				proplists:delete(max_connections,
				proplists:delete(shutdown,
				proplists:delete(socket, TransOpts))))),
			case Transport:listen(TransOpts2) of
				{ok, Socket} -> Socket;
				{error, Reason} -> listen_error(Ref, Transport, TransOpts2, Reason)
			end;
		Socket ->
			Socket
	end,
	{ok, Addr} = Transport:sockname(LSocket),
	ranch_server:set_addr(Ref, Addr),
	Procs = [
		{{acceptor, self(), N}, {ranch_acceptor, start_link, [
			LSocket, Transport, ConnsSup
		]}, permanent, brutal_kill, worker, []}
			|| N <- lists:seq(1, NumAcceptors)],
	{ok, {{one_for_one, 1, 5}, Procs}}.

-spec listen_error(any(), module(), any(), atom()) -> no_return().
listen_error(Ref, Transport, TransOpts0, Reason) ->
	TransOpts1 = lists:keyreplace(cert, 1, TransOpts0, {cert, '...'}),
	TransOpts = lists:keyreplace(key, 1, TransOpts1, {key, '...'}),
	error_logger:error_msg(
		"Failed to start Ranch listener ~p in ~p:listen(~999999p) for reason ~p (~s)~n",
		[Ref, Transport, TransOpts, Reason, format_error(Reason)]),
	exit({listen_error, Ref, Reason}).

format_error(no_cert) ->
	"no certificate provided; see cert, certfile, sni_fun or sni_hosts options";
format_error(Reason) ->
	inet:format_error(Reason).
