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

%% Make sure to never reload this module outside a release upgrade,
%% as calling l(ranch_conns_sup) twice will kill the process and all
%% the currently open connections.
-module(ranch_conns_sup).

%% API.
-export([start_link/6]).
-export([start_protocol/2]).
-export([active_connections/1]).

%% Supervisor internals.
-export([init/7]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type conn_type() :: worker | supervisor.
-type shutdown() :: brutal_kill | timeout().

-record(state, {
	parent = undefined :: pid(),
	ref :: ranch:ref(),
	conn_type :: conn_type(),
	shutdown :: shutdown(),
	transport = undefined :: module(),
	protocol = undefined :: module(),
	opts :: any(),
	ack_timeout :: timeout(),
	max_conns = undefined :: ranch:max_conns()
}).

%% API.

-spec start_link(ranch:ref(), conn_type(), shutdown(), module(),
	timeout(), module()) -> {ok, pid()}.
start_link(Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol) ->
	proc_lib:start_link(?MODULE, init,
		[self(), Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol]).

%% We can safely assume we are on the same node as the supervisor.
%%
%% We can also safely avoid having a monitor and a timeout here
%% because only three things can happen:
%%  *  The supervisor died; rest_for_one strategy killed all acceptors
%%     so this very calling process is going to di--
%%  *  There's too many connections, the supervisor will resume the
%%     acceptor only when we get below the limit again.
%%  *  The supervisor is overloaded, there's either too many acceptors
%%     or the max_connections limit is too large. It's better if we
%%     don't keep accepting connections because this leaves
%%     more room for the situation to be resolved.
%%
%% We do not need the reply, we only need the ok from the supervisor
%% to continue. The supervisor sends its own pid when the acceptor can
%% continue.
-spec start_protocol(pid(), inet:socket()) -> ok.
start_protocol(SupPid, Socket) ->
	SupPid ! {?MODULE, start_protocol, self(), Socket},
	receive SupPid -> ok end.

%% We can't make the above assumptions here. This function might be
%% called from anywhere.
-spec active_connections(pid()) -> non_neg_integer().
active_connections(SupPid) ->
	Tag = erlang:monitor(process, SupPid),
	catch erlang:send(SupPid, {?MODULE, active_connections, self(), Tag},
		[noconnect]),
	receive
		{Tag, Ret} ->
			erlang:demonitor(Tag, [flush]),
			Ret;
		{'DOWN', Tag, _, _, noconnection} ->
			exit({nodedown, node(SupPid)});
		{'DOWN', Tag, _, _, Reason} ->
			exit(Reason)
	after 5000 ->
		erlang:demonitor(Tag, [flush]),
		exit(timeout)
	end.

%% Supervisor internals.

-spec init(pid(), ranch:ref(), conn_type(), shutdown(),
	module(), timeout(), module()) -> no_return().
init(Parent, Ref, ConnType, Shutdown, Transport, AckTimeout, Protocol) ->
	process_flag(trap_exit, true),
	ok = ranch_server:set_connections_sup(Ref, self()),
	MaxConns = ranch_server:get_max_connections(Ref),
	Opts = ranch_server:get_protocol_options(Ref),
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	loop(#state{parent=Parent, ref=Ref, conn_type=ConnType,
		shutdown=Shutdown, transport=Transport, protocol=Protocol,
		opts=Opts, ack_timeout=AckTimeout, max_conns=MaxConns}, 0, 0, []).

loop(State=#state{parent=Parent, ref=Ref, conn_type=ConnType,
		transport=Transport, protocol=Protocol, opts=Opts,
		max_conns=MaxConns}, CurConns, NbChildren, Sleepers) ->
	receive
		{?MODULE, start_protocol, To, Socket} ->
			try Protocol:start_link(Ref, Socket, Transport, Opts) of
				{ok, Pid} ->
					shoot(State, CurConns, NbChildren, Sleepers, To, Socket, Pid, Pid);
				{ok, SupPid, ProtocolPid} when ConnType =:= supervisor ->
					shoot(State, CurConns, NbChildren, Sleepers, To, Socket, SupPid, ProtocolPid);
				Ret ->
					To ! self(),
					error_logger:error_msg(
						"Ranch listener ~p connection process start failure; "
						"~p:start_link/4 returned: ~999999p~n",
						[Ref, Protocol, Ret]),
					Transport:close(Socket),
					loop(State, CurConns, NbChildren, Sleepers)
			catch Class:Reason ->
				To ! self(),
				error_logger:error_msg(
					"Ranch listener ~p connection process start failure; "
					"~p:start_link/4 crashed with reason: ~p:~999999p~n",
					[Ref, Protocol, Class, Reason]),
				loop(State, CurConns, NbChildren, Sleepers)
			end;
		{?MODULE, active_connections, To, Tag} ->
			To ! {Tag, CurConns},
			loop(State, CurConns, NbChildren, Sleepers);
		%% Remove a connection from the count of connections.
		{remove_connection, Ref, Pid} ->
			case put(Pid, removed) of
				active ->
					loop(State, CurConns - 1, NbChildren, Sleepers);
				remove ->
					loop(State, CurConns, NbChildren, Sleepers);
				undefined ->
					_ = erase(Pid),
					loop(State, CurConns, NbChildren, Sleepers)
			end;
		%% Upgrade the max number of connections allowed concurrently.
		%% We resume all sleeping acceptors if this number increases.
		{set_max_conns, MaxConns2} when MaxConns2 > MaxConns ->
			_ = [To ! self() || To <- Sleepers],
			loop(State#state{max_conns=MaxConns2},
				CurConns, NbChildren, []);
		{set_max_conns, MaxConns2} ->
			loop(State#state{max_conns=MaxConns2},
				CurConns, NbChildren, Sleepers);
		%% Upgrade the protocol options.
		{set_opts, Opts2} ->
			loop(State#state{opts=Opts2},
				CurConns, NbChildren, Sleepers);
		{'EXIT', Parent, Reason} ->
			terminate(State, Reason, NbChildren);
		{'EXIT', Pid, Reason} when Sleepers =:= [] ->
			case erase(Pid) of
				active ->
					report_error(Ref, Protocol, Pid, Reason),
					loop(State, CurConns - 1, NbChildren - 1, Sleepers);
				removed ->
					report_error(Ref, Protocol, Pid, Reason),
					loop(State, CurConns, NbChildren - 1, Sleepers);
				undefined ->
					loop(State, CurConns, NbChildren, Sleepers)
			end;
		%% Resume a sleeping acceptor if needed.
		{'EXIT', Pid, Reason} ->
			case erase(Pid) of
				active when CurConns > MaxConns ->
					report_error(Ref, Protocol, Pid, Reason),
					loop(State, CurConns - 1, NbChildren - 1, Sleepers);
				active ->
					report_error(Ref, Protocol, Pid, Reason),
					[To|Sleepers2] = Sleepers,
					To ! self(),
					loop(State, CurConns - 1, NbChildren - 1, Sleepers2);
				removed ->
					report_error(Ref, Protocol, Pid, Reason),
					loop(State, CurConns, NbChildren - 1, Sleepers);
				undefined ->
					loop(State, CurConns, NbChildren, Sleepers)
			end;
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				{State, CurConns, NbChildren, Sleepers});
		%% Calls from the supervisor module.
		{'$gen_call', {To, Tag}, which_children} ->
			Children = [{Protocol, Pid, ConnType, [Protocol]}
				|| {Pid, Type} <- get(),
				Type =:= active orelse Type =:= removed],
			To ! {Tag, Children},
			loop(State, CurConns, NbChildren, Sleepers);
		{'$gen_call', {To, Tag}, count_children} ->
			Counts = case ConnType of
				worker -> [{supervisors, 0}, {workers, NbChildren}];
				supervisor -> [{supervisors, NbChildren}, {workers, 0}]
			end,
			Counts2 = [{specs, 1}, {active, NbChildren}|Counts],
			To ! {Tag, Counts2},
			loop(State, CurConns, NbChildren, Sleepers);
		{'$gen_call', {To, Tag}, _} ->
			To ! {Tag, {error, ?MODULE}},
			loop(State, CurConns, NbChildren, Sleepers);
		Msg ->
			error_logger:error_msg(
				"Ranch listener ~p received unexpected message ~p~n",
				[Ref, Msg])
	end.

shoot(State=#state{ref=Ref, transport=Transport, ack_timeout=AckTimeout, max_conns=MaxConns},
		CurConns, NbChildren, Sleepers, To, Socket, SupPid, ProtocolPid) ->
	case Transport:controlling_process(Socket, ProtocolPid) of
		ok ->
			ProtocolPid ! {shoot, Ref, Transport, Socket, AckTimeout},
			put(SupPid, active),
			CurConns2 = CurConns + 1,
			if CurConns2 < MaxConns ->
					To ! self(),
					loop(State, CurConns2, NbChildren + 1, Sleepers);
				true ->
					loop(State, CurConns2, NbChildren + 1, [To|Sleepers])
			end;
		{error, _} ->
			Transport:close(Socket),
			%% Only kill the supervised pid, because the connection's pid,
			%% when different, is supposed to be sitting under it and linked.
			exit(SupPid, kill),
			To ! self(),
			loop(State, CurConns, NbChildren, Sleepers)
	end.

-spec terminate(#state{}, any(), non_neg_integer()) -> no_return().
terminate(#state{shutdown=brutal_kill}, Reason, _) ->
	kill_children(get_keys(active)),
	kill_children(get_keys(removed)),
	exit(Reason);
%% Attempt to gracefully shutdown all children.
terminate(#state{shutdown=Shutdown}, Reason, NbChildren) ->
	shutdown_children(get_keys(active)),
	shutdown_children(get_keys(removed)),
	_ = if
		Shutdown =:= infinity ->
			ok;
		true ->
			erlang:send_after(Shutdown, self(), kill)
	end,
	wait_children(NbChildren),
	exit(Reason).

%% Kill all children and then exit. We unlink first to avoid
%% getting a message for each child getting killed.
kill_children(Pids) ->
	_ = [begin
		unlink(P),
		exit(P, kill)
	end || P <- Pids],
	ok.

%% Monitor processes so we can know which ones have shutdown
%% before the timeout. Unlink so we avoid receiving an extra
%% message. Then send a shutdown exit signal.
shutdown_children(Pids) ->
	_ = [begin
		monitor(process, P),
		unlink(P),
		exit(P, shutdown)
	end || P <- Pids],
	ok.

wait_children(0) ->
	ok;
wait_children(NbChildren) ->
	receive
        {'DOWN', _, process, Pid, _} ->
			case erase(Pid) of
				active -> wait_children(NbChildren - 1);
				removed -> wait_children(NbChildren - 1);
				_ -> wait_children(NbChildren)
			end;
		kill ->
			Active = get_keys(active),
			_ = [exit(P, kill) || P <- Active],
			Removed = get_keys(removed),
			_ = [exit(P, kill) || P <- Removed],
			ok
	end.

system_continue(_, _, {State, CurConns, NbChildren, Sleepers}) ->
	loop(State, CurConns, NbChildren, Sleepers).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, {State, _, NbChildren, _}) ->
	terminate(State, Reason, NbChildren).

system_code_change(Misc, _, _, _) ->
	{ok, Misc}.

%% We use ~999999p here instead of ~w because the latter doesn't
%% support printable strings.
report_error(_, _, _, normal) ->
	ok;
report_error(_, _, _, shutdown) ->
	ok;
report_error(_, _, _, {shutdown, _}) ->
	ok;
report_error(Ref, Protocol, Pid, Reason) ->
	error_logger:error_msg(
		"Ranch listener ~p had connection process started with "
		"~p:start_link/4 at ~p exit with reason: ~999999p~n",
		[Ref, Protocol, Pid, Reason]).
