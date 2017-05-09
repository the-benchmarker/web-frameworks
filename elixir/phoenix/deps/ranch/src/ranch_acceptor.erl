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

-module(ranch_acceptor).

-export([start_link/3]).
-export([loop/3]).

-spec start_link(inet:socket(), module(), pid())
	-> {ok, pid()}.
start_link(LSocket, Transport, ConnsSup) ->
	Pid = spawn_link(?MODULE, loop, [LSocket, Transport, ConnsSup]),
	{ok, Pid}.

-spec loop(inet:socket(), module(), pid()) -> no_return().
loop(LSocket, Transport, ConnsSup) ->
	_ = case Transport:accept(LSocket, infinity) of
		{ok, CSocket} ->
			case Transport:controlling_process(CSocket, ConnsSup) of
				ok ->
					%% This call will not return until process has been started
					%% AND we are below the maximum number of connections.
					ranch_conns_sup:start_protocol(ConnsSup, CSocket);
				{error, _} ->
					Transport:close(CSocket)
			end;
		%% Reduce the accept rate if we run out of file descriptors.
		%% We can't accept anymore anyway, so we might as well wait
		%% a little for the situation to resolve itself.
		{error, emfile} ->
			error_logger:warning_msg("Ranch acceptor reducing accept rate: out of file descriptors~n"),
			receive after 100 -> ok end;
		%% We want to crash if the listening socket got closed.
		{error, Reason} when Reason =/= closed ->
			ok
	end,
	flush(),
	?MODULE:loop(LSocket, Transport, ConnsSup).

flush() ->
	receive Msg ->
		error_logger:error_msg(
			"Ranch acceptor received unexpected message: ~p~n",
			[Msg]),
		flush()
	after 0 ->
		ok
	end.
