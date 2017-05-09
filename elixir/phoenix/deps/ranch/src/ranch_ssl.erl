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

-module(ranch_ssl).
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

-type ssl_opt() :: {alpn_preferred_protocols, [binary()]}
	| {beast_mitigation, one_n_minus_one | zero_n | disabled}
	| {cacertfile, string()}
	| {cacerts, [public_key:der_encoded()]}
	| {cert, public_key:der_encoded()}
	| {certfile, string()}
	| {ciphers, [ssl:erl_cipher_suite()] | string()}
	| {client_renegotiation, boolean()}
	| {crl_cache, {module(), {internal | any(), list()}}}
	| {crl_check, boolean() | peer | best_effort}
	| {depth, 0..255}
	| {dh, public_key:der_encoded()}
	| {dhfile, string()}
	| {fail_if_no_peer_cert, boolean()}
	| {hibernate_after, integer() | undefined}
	| {honor_cipher_order, boolean()}
	| {key, {'RSAPrivateKey' | 'DSAPrivateKey' | 'PrivateKeyInfo', public_key:der_encoded()}}
	| {keyfile, string()}
	| {log_alert, boolean()}
	| {next_protocols_advertised, [binary()]}
	| {padding_check, boolean()}
	| {partial_chain, fun(([public_key:der_encoded()]) -> {trusted_ca, public_key:der_encoded()} | unknown_ca)}
	| {password, string()}
	| {psk_identity, string()}
	| {reuse_session, fun()}
	| {reuse_sessions, boolean()}
	| {secure_renegotiate, boolean()}
	| {signature_algs, [{atom(), atom()}]}
	| {sni_fun, fun()}
	| {sni_hosts, [{string(), ssl_opt()}]}
	| {user_lookup_fun, {fun(), any()}}
	| {v2_hello_compatible, boolean()}
	| {verify, ssl:verify_type()}
	| {verify_fun, {fun(), any()}}
	| {versions, [atom()]}.
-export_type([ssl_opt/0]).

-type opt() :: ranch_tcp:opt() | ssl_opt().
-export_type([opt/0]).

-type opts() :: [opt()].
-export_type([opts/0]).

name() -> ssl.

-spec secure() -> boolean().
secure() ->
    true.

messages() -> {ssl, ssl_closed, ssl_error}.

-spec listen(opts()) -> {ok, ssl:sslsocket()} | {error, atom()}.
listen(Opts) ->
	case lists:keymember(cert, 1, Opts)
			orelse lists:keymember(certfile, 1, Opts)
			orelse lists:keymember(sni_fun, 1, Opts)
			orelse lists:keymember(sni_hosts, 1, Opts) of
		true ->
			do_listen(Opts);
		false ->
			{error, no_cert}
	end.

do_listen(Opts) ->
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	Opts3 = ranch:set_option_default(Opts2, ciphers, unbroken_cipher_suites()),
	Opts4 = ranch:set_option_default(Opts3, nodelay, true),
	Opts5 = ranch:set_option_default(Opts4, send_timeout, 30000),
	Opts6 = ranch:set_option_default(Opts5, send_timeout_close, true),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	ssl:listen(0, ranch:filter_options(Opts6, disallowed_listen_options(),
		[binary, {active, false}, {packet, raw}, {reuseaddr, true}])).

%% 'binary' and 'list' are disallowed but they are handled
%% specifically as they do not have 2-tuple equivalents.
disallowed_listen_options() ->
	[alpn_advertised_protocols, client_preferred_next_protocols,
		fallback, server_name_indication, srp_identity
		|ranch_tcp:disallowed_listen_options()].

-spec accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	ssl:transport_accept(LSocket, Timeout).

-spec accept_ack(ssl:sslsocket(), timeout()) -> ok.
accept_ack(CSocket, Timeout) ->
	case ssl:ssl_accept(CSocket, Timeout) of
		ok ->
			ok;
		%% Garbage was most likely sent to the socket, don't error out.
		{error, {tls_alert, _}} ->
			ok = close(CSocket),
			exit(normal);
		%% Socket most likely stopped responding, don't error out.
		{error, Reason} when Reason =:= timeout; Reason =:= closed ->
			ok = close(CSocket),
			exit(normal);
		{error, Reason} ->
			ok = close(CSocket),
			error(Reason)
	end.

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}]).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}],
		Timeout).

-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

-spec send(ssl:sslsocket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) ->
	sendfile(Socket, Filename, 0, 0, []).

-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd(),
		non_neg_integer(), non_neg_integer())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
	sendfile(Socket, File, Offset, Bytes, []).

%% Unlike with TCP, no syscall can be used here, so sending files
%% through SSL will be much slower in comparison. Note that unlike
%% file:sendfile/5 this function accepts either a file or a file name.
-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd(),
		non_neg_integer(), non_neg_integer(), ranch_transport:sendfile_opts())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes, Opts) ->
	ranch_transport:sendfile(?MODULE, Socket, File, Offset, Bytes, Opts).

%% @todo Probably filter Opts?
-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

-spec sockname(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	ssl:sockname(Socket).

-spec shutdown(ssl:sslsocket(), read | write | read_write)
	-> ok | {error, atom()}.
shutdown(Socket, How) ->
	ssl:shutdown(Socket, How).

-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

%% Internal.

%% Unfortunately the implementation of elliptic-curve ciphers that has
%% been introduced in R16B01 is incomplete.  Depending on the particular
%% client, this can cause the TLS handshake to break during key
%% agreement.  Depending on the ssl application version, this function
%% returns a list of all cipher suites that are supported by default,
%% minus the elliptic-curve ones.
-spec unbroken_cipher_suites() -> [ssl:erl_cipher_suite()].
unbroken_cipher_suites() ->
	case proplists:get_value(ssl_app, ssl:versions()) of
		Version when Version =:= "5.3"; Version =:= "5.3.1" ->
			lists:filter(fun(Suite) ->
				string:left(atom_to_list(element(1, Suite)), 4) =/= "ecdh"
			end, ssl:cipher_suites());
		_ ->
			ssl:cipher_suites()
	end.
