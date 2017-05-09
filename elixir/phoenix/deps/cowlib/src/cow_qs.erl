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

-module(cow_qs).

-export([parse_qs/1]).
-export([qs/1]).
-export([urldecode/1]).
-export([urlencode/1]).

-type qs_vals() :: [{binary(), binary() | true}].

%% @doc Parse an application/x-www-form-urlencoded string.
%%
%% The percent decoding is inlined to greatly improve the performance
%% by avoiding copying binaries twice (once for extracting, once for
%% decoding) instead of just extracting the proper representation.

-spec parse_qs(binary()) -> qs_vals().
parse_qs(B) ->
	parse_qs_name(B, [], <<>>).

parse_qs_name(<< $%, H, L, Rest/bits >>, Acc, Name) ->
	C = (unhex(H) bsl 4 bor unhex(L)),
	parse_qs_name(Rest, Acc, << Name/bits, C >>);
parse_qs_name(<< $+, Rest/bits >>, Acc, Name) ->
	parse_qs_name(Rest, Acc, << Name/bits, " " >>);
parse_qs_name(<< $=, Rest/bits >>, Acc, Name) when Name =/= <<>> ->
	parse_qs_value(Rest, Acc, Name, <<>>);
parse_qs_name(<< $&, Rest/bits >>, Acc, Name) ->
	case Name of
		<<>> -> parse_qs_name(Rest, Acc, <<>>);
		_ -> parse_qs_name(Rest, [{Name, true}|Acc], <<>>)
	end;
parse_qs_name(<< C, Rest/bits >>, Acc, Name) when C =/= $%, C =/= $= ->
	parse_qs_name(Rest, Acc, << Name/bits, C >>);
parse_qs_name(<<>>, Acc, Name) ->
	case Name of
		<<>> -> lists:reverse(Acc);
		_ -> lists:reverse([{Name, true}|Acc])
	end.

parse_qs_value(<< $%, H, L, Rest/bits >>, Acc, Name, Value) ->
	C = (unhex(H) bsl 4 bor unhex(L)),
	parse_qs_value(Rest, Acc, Name, << Value/bits, C >>);
parse_qs_value(<< $+, Rest/bits >>, Acc, Name, Value) ->
	parse_qs_value(Rest, Acc, Name, << Value/bits, " " >>);
parse_qs_value(<< $&, Rest/bits >>, Acc, Name, Value) ->
	parse_qs_name(Rest, [{Name, Value}|Acc], <<>>);
parse_qs_value(<< C, Rest/bits >>, Acc, Name, Value) when C =/= $% ->
	parse_qs_value(Rest, Acc, Name, << Value/bits, C >>);
parse_qs_value(<<>>, Acc, Name, Value) ->
	lists:reverse([{Name, Value}|Acc]).

-ifdef(TEST).
parse_qs_test_() ->
	Tests = [
		{<<>>, []},
		{<<"&">>, []},
		{<<"a">>, [{<<"a">>, true}]},
		{<<"a&">>, [{<<"a">>, true}]},
		{<<"&a">>, [{<<"a">>, true}]},
		{<<"a&b">>, [{<<"a">>, true}, {<<"b">>, true}]},
		{<<"a&&b">>, [{<<"a">>, true}, {<<"b">>, true}]},
		{<<"a&b&">>, [{<<"a">>, true}, {<<"b">>, true}]},
		{<<"=">>, error},
		{<<"=b">>, error},
		{<<"a=">>, [{<<"a">>, <<>>}]},
		{<<"a=b">>, [{<<"a">>, <<"b">>}]},
		{<<"a=&b=">>, [{<<"a">>, <<>>}, {<<"b">>, <<>>}]},
		{<<"a=b&c&d=e">>, [{<<"a">>, <<"b">>},
			{<<"c">>, true}, {<<"d">>, <<"e">>}]},
		{<<"a=b=c&d=e=f&g=h=i">>, [{<<"a">>, <<"b=c">>},
			{<<"d">>, <<"e=f">>}, {<<"g">>, <<"h=i">>}]},
		{<<"+">>, [{<<" ">>, true}]},
		{<<"+=+">>, [{<<" ">>, <<" ">>}]},
		{<<"a+b=c+d">>, [{<<"a b">>, <<"c d">>}]},
		{<<"+a+=+b+&+c+=+d+">>, [{<<" a ">>, <<" b ">>},
			{<<" c ">>, <<" d ">>}]},
		{<<"a%20b=c%20d">>, [{<<"a b">>, <<"c d">>}]},
		{<<"%25%26%3D=%25%26%3D&_-.=.-_">>, [{<<"%&=">>, <<"%&=">>},
			{<<"_-.">>, <<".-_">>}]},
		{<<"for=extend%2Franch">>, [{<<"for">>, <<"extend/ranch">>}]}
	],
	[{Qs, fun() ->
		E = try parse_qs(Qs) of
			R -> R
		catch _:_ ->
			error
		end
	end} || {Qs, E} <- Tests].

parse_qs_identity_test_() ->
	Tests = [
		<<"+">>,
		<<"hl=en&q=erlang+cowboy">>,
		<<"direction=desc&for=extend%2Franch&sort=updated&state=open">>,
		<<"i=EWiIXmPj5gl6&v=QowBp0oDLQXdd4x_GwiywA&ip=98.20.31.81&"
			"la=en&pg=New8.undertonebrandsafe.com%2F698a2525065ee2"
			"60c0b2f2aaad89ab82&re=&sz=1&fc=1&fr=140&br=3&bv=11.0."
			"696.16&os=3&ov=&rs=vpl&k=cookies%7Csale%7Cbrowser%7Cm"
			"ore%7Cprivacy%7Cstatistics%7Cactivities%7Cauction%7Ce"
			"mail%7Cfree%7Cin...&t=112373&xt=5%7C61%7C0&tz=-1&ev=x"
			"&tk=&za=1&ortb-za=1&zu=&zl=&ax=U&ay=U&ortb-pid=536454"
			".55&ortb-sid=112373.8&seats=999&ortb-xt=IAB24&ortb-ugc=">>,
		<<"i=9pQNskA&v=0ySQQd1F&ev=12345678&t=12345&sz=3&ip=67.58."
			"236.89&la=en&pg=http%3A%2F%2Fwww.yahoo.com%2Fpage1.ht"
			"m&re=http%3A%2F%2Fsearch.google.com&fc=1&fr=1&br=2&bv"
			"=3.0.14&os=1&ov=XP&k=cars%2Cford&rs=js&xt=5%7C22%7C23"
			"4&tz=%2B180&tk=key1%3Dvalue1%7Ckey2%3Dvalue2&zl=4%2C5"
			"%2C6&za=4&zu=competitor.com&ua=Mozilla%2F5.0+%28Windo"
			"ws%3B+U%3B+Windows+NT+6.1%3B+en-US%29+AppleWebKit%2F5"
			"34.13+%28KHTML%2C+like+Gecko%29+Chrome%2F9.0.597.98+S"
			"afari%2F534.13&ortb-za=1%2C6%2C13&ortb-pid=521732&ort"
			"b-sid=521732&ortb-xt=IAB3&ortb-ugc=">>
	],
	[{V, fun() -> V = qs(parse_qs(V)) end} || V <- Tests].
-endif.

-ifdef(PERF).
horse_parse_qs_shorter() ->
	horse:repeat(20000,
		parse_qs(<<"hl=en&q=erlang%20cowboy">>)
	).

horse_parse_qs_short() ->
	horse:repeat(20000,
		parse_qs(
			<<"direction=desc&for=extend%2Franch&sort=updated&state=open">>)
	).

horse_parse_qs_long() ->
	horse:repeat(20000,
		parse_qs(<<"i=EWiIXmPj5gl6&v=QowBp0oDLQXdd4x_GwiywA&ip=98.20.31.81&"
			"la=en&pg=New8.undertonebrandsafe.com%2F698a2525065ee260c0b2f2a"
			"aad89ab82&re=&sz=1&fc=1&fr=140&br=3&bv=11.0.696.16&os=3&ov=&rs"
			"=vpl&k=cookies%7Csale%7Cbrowser%7Cmore%7Cprivacy%7Cstatistics%"
			"7Cactivities%7Cauction%7Cemail%7Cfree%7Cin...&t=112373&xt=5%7C"
			"61%7C0&tz=-1&ev=x&tk=&za=1&ortb-za=1&zu=&zl=&ax=U&ay=U&ortb-pi"
			"d=536454.55&ortb-sid=112373.8&seats=999&ortb-xt=IAB24&ortb-ugc"
			"=">>)
	).

horse_parse_qs_longer() ->
	horse:repeat(20000,
		parse_qs(<<"i=9pQNskA&v=0ySQQd1F&ev=12345678&t=12345&sz=3&ip=67.58."
			"236.89&la=en&pg=http%3A%2F%2Fwww.yahoo.com%2Fpage1.htm&re=http"
			"%3A%2F%2Fsearch.google.com&fc=1&fr=1&br=2&bv=3.0.14&os=1&ov=XP"
			"&k=cars%2cford&rs=js&xt=5%7c22%7c234&tz=%2b180&tk=key1%3Dvalue"
			"1%7Ckey2%3Dvalue2&zl=4,5,6&za=4&zu=competitor.com&ua=Mozilla%2"
			"F5.0%20(Windows%3B%20U%3B%20Windows%20NT%206.1%3B%20en-US)%20A"
			"ppleWebKit%2F534.13%20(KHTML%2C%20like%20Gecko)%20Chrome%2F9.0"
			".597.98%20Safari%2F534.13&ortb-za=1%2C6%2C13&ortb-pid=521732&o"
			"rtb-sid=521732&ortb-xt=IAB3&ortb-ugc=">>)
	).
-endif.

%% @doc Build an application/x-www-form-urlencoded string.

-spec qs(qs_vals()) -> binary().
qs([]) ->
	<<>>;
qs(L) ->
	qs(L, <<>>).

qs([], Acc) ->
	<< $&, Qs/bits >> = Acc,
	Qs;
qs([{Name, true}|Tail], Acc) ->
	Acc2 = urlencode(Name, << Acc/bits, $& >>),
	qs(Tail, Acc2);
qs([{Name, Value}|Tail], Acc) ->
	Acc2 = urlencode(Name, << Acc/bits, $& >>),
	Acc3 = urlencode(Value, << Acc2/bits, $= >>),
	qs(Tail, Acc3).

-define(QS_SHORTER, [
	{<<"hl">>, <<"en">>},
	{<<"q">>, <<"erlang cowboy">>}
]).

-define(QS_SHORT, [
	{<<"direction">>, <<"desc">>},
	{<<"for">>, <<"extend/ranch">>},
	{<<"sort">>, <<"updated">>},
	{<<"state">>, <<"open">>}
]).

-define(QS_LONG, [
	{<<"i">>, <<"EWiIXmPj5gl6">>},
	{<<"v">>, <<"QowBp0oDLQXdd4x_GwiywA">>},
	{<<"ip">>, <<"98.20.31.81">>},
	{<<"la">>, <<"en">>},
	{<<"pg">>, <<"New8.undertonebrandsafe.com/"
		"698a2525065ee260c0b2f2aaad89ab82">>},
	{<<"re">>, <<>>},
	{<<"sz">>, <<"1">>},
	{<<"fc">>, <<"1">>},
	{<<"fr">>, <<"140">>},
	{<<"br">>, <<"3">>},
	{<<"bv">>, <<"11.0.696.16">>},
	{<<"os">>, <<"3">>},
	{<<"ov">>, <<>>},
	{<<"rs">>, <<"vpl">>},
	{<<"k">>, <<"cookies|sale|browser|more|privacy|statistics|"
		"activities|auction|email|free|in...">>},
	{<<"t">>, <<"112373">>},
	{<<"xt">>, <<"5|61|0">>},
	{<<"tz">>, <<"-1">>},
	{<<"ev">>, <<"x">>},
	{<<"tk">>, <<>>},
	{<<"za">>, <<"1">>},
	{<<"ortb-za">>, <<"1">>},
	{<<"zu">>, <<>>},
	{<<"zl">>, <<>>},
	{<<"ax">>, <<"U">>},
	{<<"ay">>, <<"U">>},
	{<<"ortb-pid">>, <<"536454.55">>},
	{<<"ortb-sid">>, <<"112373.8">>},
	{<<"seats">>, <<"999">>},
	{<<"ortb-xt">>, <<"IAB24">>},
	{<<"ortb-ugc">>, <<>>}
]).

-define(QS_LONGER, [
	{<<"i">>, <<"9pQNskA">>},
	{<<"v">>, <<"0ySQQd1F">>},
	{<<"ev">>, <<"12345678">>},
	{<<"t">>, <<"12345">>},
	{<<"sz">>, <<"3">>},
	{<<"ip">>, <<"67.58.236.89">>},
	{<<"la">>, <<"en">>},
	{<<"pg">>, <<"http://www.yahoo.com/page1.htm">>},
	{<<"re">>, <<"http://search.google.com">>},
	{<<"fc">>, <<"1">>},
	{<<"fr">>, <<"1">>},
	{<<"br">>, <<"2">>},
	{<<"bv">>, <<"3.0.14">>},
	{<<"os">>, <<"1">>},
	{<<"ov">>, <<"XP">>},
	{<<"k">>, <<"cars,ford">>},
	{<<"rs">>, <<"js">>},
	{<<"xt">>, <<"5|22|234">>},
	{<<"tz">>, <<"+180">>},
	{<<"tk">>, <<"key1=value1|key2=value2">>},
	{<<"zl">>, <<"4,5,6">>},
	{<<"za">>, <<"4">>},
	{<<"zu">>, <<"competitor.com">>},
	{<<"ua">>, <<"Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) "
		"AppleWebKit/534.13 (KHTML, like Gecko) Chrome/9.0.597.98 "
		"Safari/534.13">>},
	{<<"ortb-za">>, <<"1,6,13">>},
	{<<"ortb-pid">>, <<"521732">>},
	{<<"ortb-sid">>, <<"521732">>},
	{<<"ortb-xt">>, <<"IAB3">>},
	{<<"ortb-ugc">>, <<>>}
]).

-ifdef(TEST).
qs_test_() ->
	Tests = [
		{[<<"a">>], error},
		{[{<<"a">>, <<"b">>, <<"c">>}], error},
		{[], <<>>},
		{[{<<"a">>, true}], <<"a">>},
		{[{<<"a">>, true}, {<<"b">>, true}], <<"a&b">>},
		{[{<<"a">>, <<>>}], <<"a=">>},
		{[{<<"a">>, <<"b">>}], <<"a=b">>},
		{[{<<"a">>, <<>>}, {<<"b">>, <<>>}], <<"a=&b=">>},
		{[{<<"a">>, <<"b">>}, {<<"c">>, true}, {<<"d">>, <<"e">>}],
			<<"a=b&c&d=e">>},
		{[{<<"a">>, <<"b=c">>}, {<<"d">>, <<"e=f">>}, {<<"g">>, <<"h=i">>}],
			<<"a=b%3Dc&d=e%3Df&g=h%3Di">>},
		{[{<<" ">>, true}], <<"+">>},
		{[{<<" ">>, <<" ">>}], <<"+=+">>},
		{[{<<"a b">>, <<"c d">>}], <<"a+b=c+d">>},
		{[{<<" a ">>, <<" b ">>}, {<<" c ">>, <<" d ">>}],
			<<"+a+=+b+&+c+=+d+">>},
		{[{<<"%&=">>, <<"%&=">>}, {<<"_-.">>, <<".-_">>}],
			<<"%25%26%3D=%25%26%3D&_-.=.-_">>},
		{[{<<"for">>, <<"extend/ranch">>}], <<"for=extend%2Franch">>}
	],
	[{lists:flatten(io_lib:format("~p", [Vals])), fun() ->
		E = try qs(Vals) of
			R -> R
		catch _:_ ->
			error
		end
	end} || {Vals, E} <- Tests].

qs_identity_test_() ->
	Tests = [
		[{<<"+">>, true}],
		?QS_SHORTER,
		?QS_SHORT,
		?QS_LONG,
		?QS_LONGER
	],
	[{lists:flatten(io_lib:format("~p", [V])), fun() ->
		V = parse_qs(qs(V))
	end} || V <- Tests].
-endif.

-ifdef(PERF).
horse_qs_shorter() ->
	horse:repeat(20000, qs(?QS_SHORTER)).

horse_qs_short() ->
	horse:repeat(20000, qs(?QS_SHORT)).

horse_qs_long() ->
	horse:repeat(20000, qs(?QS_LONG)).

horse_qs_longer() ->
	horse:repeat(20000, qs(?QS_LONGER)).
-endif.

%% @doc Decode a percent encoded string (x-www-form-urlencoded rules).

-spec urldecode(B) -> B when B::binary().
urldecode(B) ->
	urldecode(B, <<>>).

urldecode(<< $%, H, L, Rest/bits >>, Acc) ->
	C = (unhex(H) bsl 4 bor unhex(L)),
	urldecode(Rest, << Acc/bits, C >>);
urldecode(<< $+, Rest/bits >>, Acc) ->
	urldecode(Rest, << Acc/bits, " " >>);
urldecode(<< C, Rest/bits >>, Acc) when C =/= $% ->
	urldecode(Rest, << Acc/bits, C >>);
urldecode(<<>>, Acc) ->
	Acc.

unhex($0) ->  0;
unhex($1) ->  1;
unhex($2) ->  2;
unhex($3) ->  3;
unhex($4) ->  4;
unhex($5) ->  5;
unhex($6) ->  6;
unhex($7) ->  7;
unhex($8) ->  8;
unhex($9) ->  9;
unhex($A) -> 10;
unhex($B) -> 11;
unhex($C) -> 12;
unhex($D) -> 13;
unhex($E) -> 14;
unhex($F) -> 15;
unhex($a) -> 10;
unhex($b) -> 11;
unhex($c) -> 12;
unhex($d) -> 13;
unhex($e) -> 14;
unhex($f) -> 15.

-ifdef(TEST).
urldecode_test_() ->
	Tests = [
		{<<"%20">>, <<" ">>},
		{<<"+">>, <<" ">>},
		{<<"%00">>, <<0>>},
		{<<"%fF">>, <<255>>},
		{<<"123">>, <<"123">>},
		{<<"%i5">>, error},
		{<<"%5">>, error}
	],
	[{Qs, fun() ->
		E = try urldecode(Qs) of
			R -> R
		catch _:_ ->
			error
		end
	end} || {Qs, E} <- Tests].

urldecode_identity_test_() ->
	Tests = [
		<<"+">>,
		<<"nothingnothingnothingnothing">>,
		<<"Small+fast+modular+HTTP+server">>,
		<<"Small%2C+fast%2C+modular+HTTP+server.">>,
		<<"%E3%83%84%E3%82%A4%E3%83%B3%E3%82%BD%E3%82%A6%E3%83"
			"%AB%E3%80%9C%E8%BC%AA%E5%BB%BB%E3%81%99%E3%82%8B%E6%97%8B%E5"
			"%BE%8B%E3%80%9C">>
	],
	[{V, fun() -> V = urlencode(urldecode(V)) end} || V <- Tests].
-endif.

-ifdef(PERF).
horse_urldecode() ->
	horse:repeat(100000,
		urldecode(<<"nothingnothingnothingnothing">>)
	).

horse_urldecode_plus() ->
	horse:repeat(100000,
		urldecode(<<"Small+fast+modular+HTTP+server">>)
	).

horse_urldecode_hex() ->
	horse:repeat(100000,
		urldecode(<<"Small%2C%20fast%2C%20modular%20HTTP%20server.">>)
	).

horse_urldecode_jp_hex() ->
	horse:repeat(100000,
		urldecode(<<"%E3%83%84%E3%82%A4%E3%83%B3%E3%82%BD%E3%82%A6%E3%83"
			"%AB%E3%80%9C%E8%BC%AA%E5%BB%BB%E3%81%99%E3%82%8B%E6%97%8B%E5"
			"%BE%8B%E3%80%9C">>)
	).

horse_urldecode_mix() ->
	horse:repeat(100000,
		urldecode(<<"Small%2C+fast%2C+modular+HTTP+server.">>)
	).
-endif.

%% @doc Percent encode a string (x-www-form-urlencoded rules).

-spec urlencode(B) -> B when B::binary().
urlencode(B) ->
	urlencode(B, <<>>).

urlencode(<< $\s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $+ >>);
urlencode(<< $-, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $- >>);
urlencode(<< $., Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $. >>);
urlencode(<< $0, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $0 >>);
urlencode(<< $1, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $1 >>);
urlencode(<< $2, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $2 >>);
urlencode(<< $3, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $3 >>);
urlencode(<< $4, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $4 >>);
urlencode(<< $5, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $5 >>);
urlencode(<< $6, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $6 >>);
urlencode(<< $7, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $7 >>);
urlencode(<< $8, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $8 >>);
urlencode(<< $9, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $9 >>);
urlencode(<< $A, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $A >>);
urlencode(<< $B, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $B >>);
urlencode(<< $C, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $C >>);
urlencode(<< $D, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $D >>);
urlencode(<< $E, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $E >>);
urlencode(<< $F, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $F >>);
urlencode(<< $G, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $G >>);
urlencode(<< $H, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $H >>);
urlencode(<< $I, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $I >>);
urlencode(<< $J, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $J >>);
urlencode(<< $K, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $K >>);
urlencode(<< $L, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $L >>);
urlencode(<< $M, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $M >>);
urlencode(<< $N, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $N >>);
urlencode(<< $O, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $O >>);
urlencode(<< $P, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $P >>);
urlencode(<< $Q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Q >>);
urlencode(<< $R, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $R >>);
urlencode(<< $S, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $S >>);
urlencode(<< $T, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $T >>);
urlencode(<< $U, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $U >>);
urlencode(<< $V, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $V >>);
urlencode(<< $W, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $W >>);
urlencode(<< $X, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $X >>);
urlencode(<< $Y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Y >>);
urlencode(<< $Z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Z >>);
urlencode(<< $_, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $_ >>);
urlencode(<< $a, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $a >>);
urlencode(<< $b, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $b >>);
urlencode(<< $c, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $c >>);
urlencode(<< $d, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $d >>);
urlencode(<< $e, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $e >>);
urlencode(<< $f, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $f >>);
urlencode(<< $g, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $g >>);
urlencode(<< $h, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $h >>);
urlencode(<< $i, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $i >>);
urlencode(<< $j, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $j >>);
urlencode(<< $k, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $k >>);
urlencode(<< $l, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $l >>);
urlencode(<< $m, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $m >>);
urlencode(<< $n, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $n >>);
urlencode(<< $o, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $o >>);
urlencode(<< $p, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $p >>);
urlencode(<< $q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $q >>);
urlencode(<< $r, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $r >>);
urlencode(<< $s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $s >>);
urlencode(<< $t, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $t >>);
urlencode(<< $u, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $u >>);
urlencode(<< $v, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $v >>);
urlencode(<< $w, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $w >>);
urlencode(<< $x, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $x >>);
urlencode(<< $y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $y >>);
urlencode(<< $z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $z >>);
urlencode(<< C, Rest/bits >>, Acc) ->
	H = hex(C bsr 4),
	L = hex(C band 16#0f),
	urlencode(Rest, << Acc/bits, $%, H, L >>);
urlencode(<<>>, Acc) ->
	Acc.

hex( 0) -> $0;
hex( 1) -> $1;
hex( 2) -> $2;
hex( 3) -> $3;
hex( 4) -> $4;
hex( 5) -> $5;
hex( 6) -> $6;
hex( 7) -> $7;
hex( 8) -> $8;
hex( 9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.

-ifdef(TEST).
urlencode_test_() ->
	Tests = [
		{<<255, 0>>, <<"%FF%00">>},
		{<<255, " ">>, <<"%FF+">>},
		{<<" ">>, <<"+">>},
		{<<"aBc123">>, <<"aBc123">>},
		{<<".-_">>, <<".-_">>}
	],
	[{V, fun() -> E = urlencode(V) end} || {V, E} <- Tests].

urlencode_identity_test_() ->
	Tests = [
		<<"+">>,
		<<"nothingnothingnothingnothing">>,
		<<"Small fast modular HTTP server">>,
		<<"Small, fast, modular HTTP server.">>,
		<<227,131,132,227,130,164,227,131,179,227,130,189,227,
			130,166,227,131,171,227,128,156,232,188,170,229,187,187,227,
			129,153,227,130,139,230,151,139,229,190,139,227,128,156>>
	],
	[{V, fun() -> V = urldecode(urlencode(V)) end} || V <- Tests].
-endif.

-ifdef(PERF).
horse_urlencode() ->
	horse:repeat(100000,
		urlencode(<<"nothingnothingnothingnothing">>)
	).

horse_urlencode_plus() ->
	horse:repeat(100000,
		urlencode(<<"Small fast modular HTTP server">>)
	).

horse_urlencode_jp() ->
	horse:repeat(100000,
		urlencode(<<227,131,132,227,130,164,227,131,179,227,130,189,227,
			130,166,227,131,171,227,128,156,232,188,170,229,187,187,227,
			129,153,227,130,139,230,151,139,229,190,139,227,128,156>>)
	).

horse_urlencode_mix() ->
	horse:repeat(100000,
		urlencode(<<"Small, fast, modular HTTP server.">>)
	).
-endif.
