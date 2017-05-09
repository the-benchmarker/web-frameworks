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

-module(cow_cookie).

-export([parse_cookie/1]).
-export([setcookie/3]).

-type cookie_option() :: {max_age, non_neg_integer()}
	| {domain, binary()} | {path, binary()}
	| {secure, boolean()} | {http_only, boolean()}.
-type cookie_opts() :: [cookie_option()].
-export_type([cookie_opts/0]).

%% @doc Parse a cookie header string and return a list of key/values.

-spec parse_cookie(binary()) -> [{binary(), binary()}] | {error, badarg}.
parse_cookie(Cookie) ->
	parse_cookie(Cookie, []).

parse_cookie(<<>>, Acc) ->
	lists:reverse(Acc);
parse_cookie(<< $\s, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
parse_cookie(<< $\t, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
parse_cookie(<< $,, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
parse_cookie(<< $;, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
parse_cookie(<< $$, Rest/binary >>, Acc) ->
	skip_cookie(Rest, Acc);
parse_cookie(Cookie, Acc) ->
	parse_cookie_name(Cookie, Acc, <<>>).

skip_cookie(<<>>, Acc) ->
	lists:reverse(Acc);
skip_cookie(<< $,, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
skip_cookie(<< $;, Rest/binary >>, Acc) ->
	parse_cookie(Rest, Acc);
skip_cookie(<< _, Rest/binary >>, Acc) ->
	skip_cookie(Rest, Acc).

parse_cookie_name(<<>>, _, _) ->
	{error, badarg};
parse_cookie_name(<< $=, _/binary >>, _, <<>>) ->
	{error, badarg};
parse_cookie_name(<< $=, Rest/binary >>, Acc, Name) ->
	parse_cookie_value(Rest, Acc, Name, <<>>);
parse_cookie_name(<< $,, _/binary >>, _, _) ->
	{error, badarg};
parse_cookie_name(<< $;, _/binary >>, _, _) ->
	{error, badarg};
parse_cookie_name(<< $\s, _/binary >>, _, _) ->
	{error, badarg};
parse_cookie_name(<< $\t, _/binary >>, _, _) ->
	{error, badarg};
parse_cookie_name(<< $\r, _/binary >>, _, _) ->
	{error, badarg};
parse_cookie_name(<< $\n, _/binary >>, _, _) ->
	{error, badarg};
parse_cookie_name(<< $\013, _/binary >>, _, _) ->
	{error, badarg};
parse_cookie_name(<< $\014, _/binary >>, _, _) ->
	{error, badarg};
parse_cookie_name(<< C, Rest/binary >>, Acc, Name) ->
	parse_cookie_name(Rest, Acc, << Name/binary, C >>).

parse_cookie_value(<<>>, Acc, Name, Value) ->
	lists:reverse([{Name, parse_cookie_trim(Value)}|Acc]);
parse_cookie_value(<< $;, Rest/binary >>, Acc, Name, Value) ->
	parse_cookie(Rest, [{Name, parse_cookie_trim(Value)}|Acc]);
parse_cookie_value(<< $\t, _/binary >>, _, _, _) ->
	{error, badarg};
parse_cookie_value(<< $\r, _/binary >>, _, _, _) ->
	{error, badarg};
parse_cookie_value(<< $\n, _/binary >>, _, _, _) ->
	{error, badarg};
parse_cookie_value(<< $\013, _/binary >>, _, _, _) ->
	{error, badarg};
parse_cookie_value(<< $\014, _/binary >>, _, _, _) ->
	{error, badarg};
parse_cookie_value(<< C, Rest/binary >>, Acc, Name, Value) ->
	parse_cookie_value(Rest, Acc, Name, << Value/binary, C >>).

parse_cookie_trim(Value = <<>>) ->
	Value;
parse_cookie_trim(Value) ->
	case binary:last(Value) of
		$\s ->
			Size = byte_size(Value) - 1,
			<< Value2:Size/binary, _ >> = Value,
			parse_cookie_trim(Value2);
		_ ->
			Value
	end.

-ifdef(TEST).
parse_cookie_test_() ->
	%% {Value, Result}.
	Tests = [
		{<<"name=value; name2=value2">>, [
			{<<"name">>, <<"value">>},
			{<<"name2">>, <<"value2">>}
		]},
		{<<"$Version=1; Customer=WILE_E_COYOTE; $Path=/acme">>, [
			{<<"Customer">>, <<"WILE_E_COYOTE">>}
		]},
		{<<"$Version=1; Customer=WILE_E_COYOTE; $Path=/acme; "
			"Part_Number=Rocket_Launcher_0001; $Path=/acme; "
			"Shipping=FedEx; $Path=/acme">>, [
			{<<"Customer">>, <<"WILE_E_COYOTE">>},
			{<<"Part_Number">>, <<"Rocket_Launcher_0001">>},
			{<<"Shipping">>, <<"FedEx">>}
		]},
		%% Space in value.
		{<<"foo=Thu Jul 11 2013 15:38:43 GMT+0400 (MSK)">>,
			[{<<"foo">>, <<"Thu Jul 11 2013 15:38:43 GMT+0400 (MSK)">>}]},
		%% Comma in value. Google Analytics sets that kind of cookies.
		{<<"refk=sOUZDzq2w2; sk=B602064E0139D842D620C7569640DBB4C81C45080651"
			"9CC124EF794863E10E80; __utma=64249653.825741573.1380181332.1400"
			"015657.1400019557.703; __utmb=64249653.1.10.1400019557; __utmc="
			"64249653; __utmz=64249653.1400019557.703.13.utmcsr=bluesky.chic"
			"agotribune.com|utmccn=(referral)|utmcmd=referral|utmcct=/origin"
			"als/chi-12-indispensable-digital-tools-bsi,0,0.storygallery">>, [
				{<<"refk">>, <<"sOUZDzq2w2">>},
				{<<"sk">>, <<"B602064E0139D842D620C7569640DBB4C81C45080651"
					"9CC124EF794863E10E80">>},
				{<<"__utma">>, <<"64249653.825741573.1380181332.1400"
					"015657.1400019557.703">>},
				{<<"__utmb">>, <<"64249653.1.10.1400019557">>},
				{<<"__utmc">>, <<"64249653">>},
				{<<"__utmz">>, <<"64249653.1400019557.703.13.utmcsr=bluesky.chic"
					"agotribune.com|utmccn=(referral)|utmcmd=referral|utmcct=/origin"
					"als/chi-12-indispensable-digital-tools-bsi,0,0.storygallery">>}
		]},
		%% Potential edge cases (initially from Mochiweb).
		{<<"foo=\\x">>, [{<<"foo">>, <<"\\x">>}]},
		{<<"=">>, {error, badarg}},
		{<<"  foo ; bar  ">>, {error, badarg}},
		{<<"foo=;bar=">>, [{<<"foo">>, <<>>}, {<<"bar">>, <<>>}]},
		{<<"foo=\\\";;bar ">>, {error, badarg}},
		{<<"foo=\\\";;bar=good ">>,
			[{<<"foo">>, <<"\\\"">>}, {<<"bar">>, <<"good">>}]},
		{<<"foo=\"\\\";bar">>, {error, badarg}},
		{<<>>, []},
		{<<"foo=bar , baz=wibble ">>, [{<<"foo">>, <<"bar , baz=wibble">>}]}
	],
	[{V, fun() -> R = parse_cookie(V) end} || {V, R} <- Tests].
-endif.

%% @doc Convert a cookie name, value and options to its iodata form.
%% @end
%%
%% Initially from Mochiweb:
%%   * Copyright 2007 Mochi Media, Inc.
%% Initial binary implementation:
%%   * Copyright 2011 Thomas Burdick <thomas.burdick@gmail.com>

-spec setcookie(iodata(), iodata(), cookie_opts()) -> iodata().
setcookie(Name, Value, Opts) ->
	nomatch = binary:match(iolist_to_binary(Name), [<<$=>>, <<$,>>, <<$;>>,
			<<$\s>>, <<$\t>>, <<$\r>>, <<$\n>>, <<$\013>>, <<$\014>>]),
	nomatch = binary:match(iolist_to_binary(Value), [<<$,>>, <<$;>>,
			<<$\s>>, <<$\t>>, <<$\r>>, <<$\n>>, <<$\013>>, <<$\014>>]),
	MaxAgeBin = case lists:keyfind(max_age, 1, Opts) of
		false -> <<>>;
		{_, 0} ->
			%% MSIE requires an Expires date in the past to delete a cookie.
			<<"; Expires=Thu, 01-Jan-1970 00:00:01 GMT; Max-Age=0">>;
		{_, MaxAge} when is_integer(MaxAge), MaxAge > 0 ->
			UTC = calendar:universal_time(),
			Secs = calendar:datetime_to_gregorian_seconds(UTC),
			Expires = calendar:gregorian_seconds_to_datetime(Secs + MaxAge),
			[<<"; Expires=">>, cow_date:rfc2109(Expires),
				<<"; Max-Age=">>, integer_to_list(MaxAge)]
	end,
	DomainBin = case lists:keyfind(domain, 1, Opts) of
		false -> <<>>;
		{_, Domain} -> [<<"; Domain=">>, Domain]
	end,
	PathBin = case lists:keyfind(path, 1, Opts) of
		false -> <<>>;
		{_, Path} -> [<<"; Path=">>, Path]
	end,
	SecureBin = case lists:keyfind(secure, 1, Opts) of
		false -> <<>>;
		{_, false} -> <<>>;
		{_, true} -> <<"; Secure">>
	end,
	HttpOnlyBin = case lists:keyfind(http_only, 1, Opts) of
		false -> <<>>;
		{_, false} -> <<>>;
		{_, true} -> <<"; HttpOnly">>
	end,
	[Name, <<"=">>, Value, <<"; Version=1">>,
		MaxAgeBin, DomainBin, PathBin, SecureBin, HttpOnlyBin].

-ifdef(TEST).
setcookie_test_() ->
	%% {Name, Value, Opts, Result}
	Tests = [
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			[{http_only, true}, {domain, <<"acme.com">>}],
			<<"Customer=WILE_E_COYOTE; Version=1; "
				"Domain=acme.com; HttpOnly">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			[{path, <<"/acme">>}],
			<<"Customer=WILE_E_COYOTE; Version=1; Path=/acme">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			[{secure, true}],
			<<"Customer=WILE_E_COYOTE; Version=1; Secure">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			[{secure, false}, {http_only, false}],
			<<"Customer=WILE_E_COYOTE; Version=1">>},
		{<<"Customer">>, <<"WILE_E_COYOTE">>,
			[{path, <<"/acme">>}, {badoption, <<"negatory">>}],
			<<"Customer=WILE_E_COYOTE; Version=1; Path=/acme">>}
	],
	[{R, fun() -> R = iolist_to_binary(setcookie(N, V, O)) end}
		|| {N, V, O, R} <- Tests].

setcookie_max_age_test() ->
	F = fun(N, V, O) ->
		binary:split(iolist_to_binary(
			setcookie(N, V, O)), <<";">>, [global])
	end,
	[<<"Customer=WILE_E_COYOTE">>,
		<<" Version=1">>,
		<<" Expires=", _/binary>>,
		<<" Max-Age=111">>,
		<<" Secure">>] = F(<<"Customer">>, <<"WILE_E_COYOTE">>,
			[{max_age, 111}, {secure, true}]),
	case catch F(<<"Customer">>, <<"WILE_E_COYOTE">>, [{max_age, -111}]) of
		{'EXIT', {{case_clause, {max_age, -111}}, _}} -> ok
	end,
	[<<"Customer=WILE_E_COYOTE">>,
		<<" Version=1">>,
		<<" Expires=", _/binary>>,
		<<" Max-Age=86417">>] = F(<<"Customer">>, <<"WILE_E_COYOTE">>,
			 [{max_age, 86417}]),
	ok.

setcookie_failures_test_() ->
	F = fun(N, V) ->
		try setcookie(N, V, []) of
			_ ->
				false
		catch _:_ ->
			true
		end
	end,
	Tests = [
		{<<"Na=me">>, <<"Value">>},
		{<<"Name;">>, <<"Value">>},
		{<<"\r\name">>, <<"Value">>},
		{<<"Name">>, <<"Value;">>},
		{<<"Name">>, <<"\value">>}
	],
	[{iolist_to_binary(io_lib:format("{~p, ~p} failure", [N, V])),
		fun() -> true = F(N, V) end}
		|| {N, V} <- Tests].
-endif.
